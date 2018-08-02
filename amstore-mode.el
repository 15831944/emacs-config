;;; amstore-mode.el --- a minor mode to contain functions for work -*- lexical-binding: t -*-

;; Copyright (C) 2018 K. C. Juntunen

;; Author   : K. C. Juntunen <juntunen.kc@gmail.com>
;; URL      : https://github.com/kcjuntunen/emacs-config/blob/master/amstore-mode.el
;; Version  : 0.1

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; This is a handy collection of functions for org-mode to make me a bit more productive
;; at work.

;;; Code:
(require 'org)
(require 'subr-x)
(require 'w32-browser)

(defvar amstore--stp-path "G:/STRIKER LASER PROGRAMS/STP"
  "Path to setup files for laser.")

(defvar amstore--mtl-path "S:\\shared\\general\\Metals\\METAL MANUFACTURING"
  "Path to metal department models and drawings. This is here only because it's
  convienient to copy it to the w32 clipboard all the time.")

;;;###autoload
(define-minor-mode amstore-mode
  "A container for handy, Amstore-related functions."
  :lighter " ∀"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c b") 'amstore-org-headline-w32-browser)
            (define-key map (kbd "C-c g") 'amstore-get-headline-part-runtime)
            (define-key map (kbd "C-c G") 'amstore-get-part-runtime)
            (define-key map (kbd "C-c j") 'amstore-copy-job-number-to-clipboard)
            (define-key map (kbd "C-c m") 'amstore-copy-metal-path-to-clipboard)
            (define-key map (kbd "C-c x") 'amstore-open-related-xls)
            map))

(defun amstore--org-buffer-prop (prop)
  "Return the value of a buffer property named PROP.

There has to already be a function for this, but I couldn't find it."
  (if (string-match (concat "^#\\+\\(" prop "\\):[ \t]*\\(.*\\)") (buffer-string) 0)
      (match-string 2 (buffer-string))
    nil))

(defun amstore--sanitize-path (path &optional filename)
  "Make PATH all proper."
  (cond
   (path
    (with-temp-buffer
      (insert path)
      (replace-string "\\" "/" nil (point-min) (point-max))
      (setq match (string-match "/$" (buffer-string)))
      (unless (or match filename)
        (goto-char (point-max))
        (insert "/"))
      (buffer-string)))
   (t
    nil)))

;;;###autoload
(defun amstore-org-headline-w32-browser ()
  "Look first in the `PROPERTIES' drawer for a path under `MODEL'.
Failing that, look in whatever path we can inherit, concatenate with the
heading, and try a few extensions. Failing that, ask for a filename."
  (interactive)
  (let ((to-open (or (org-entry-get (point) "MODEL" t nil)
                     (amstore--get-link-path "SLDDRW"))))
    (if (and to-open (file-exists-p to-open))
        (w32-browser to-open)
      (let ((path (amstore--sanitize-path (or (org-entry-get (point) "MDLPATH" t nil)
                                              (amstore--org-buffer-prop "MDLPATH"))))
            (exts '("SLDDRW" "SLDASM" "SLDPRT" "PDF"))
            (headingtext (cond ((string-equal (org-entry-get (point) "Type") "Request")
                                (amstore-get-heading-names))
                               (t (nth 4 (org-heading-components))))))
        (when (and path headingtext)
          (while (and exts (not (and to-open (file-exists-p to-open))))
            (setq to-open (concat path headingtext "." (car exts))
                  exts (cdr exts))))
        (if (and to-open (file-exists-p to-open))
            (progn
              (org-set-property "MODEL" to-open)
              (w32-browser to-open))
          (setq to-open (read-file-name
                         (format "Enter path of `%s': " headingtext)
                         amstore--mtl-path))
          (if (not (file-exists-p to-open))
              (error (format "File `%s' doesn't exist!" to-open))
            (org-set-property "MODEL" to-open)
            (w32-browser to-open)))))))

(defun amstore--get-link-path (prop)
  "Look for a link under PROP and return the path."
  (let ((link (org-entry-get (point) prop)))
    (when link
        (string-match "\\[\\[file:\\(.*\\)\\]\\[\\(.*\\)\\]\\]" link)
      (with-temp-buffer
        (insert (match-string 1 link))
        (replace-string "\\" "/" nil (point-min) (point-max) nil)
        (buffer-string)))))

;;;###autoload
(defun amstore-open-containing-dir ()
  "Open the path associated with this org heading."
  (interactive)
  (let ((model (org-entry-get (point) "MODEL" t nil)))
    (setq path (with-temp-buffer
                 (insert (amstore--sanitize-path model t))
                 (goto-char (point-max))
                 (set-mark (point-max))
                 (search-backward "/")
                 (forward-char)
                 (delete-active-region)
                 (buffer-string)))
    (when (and path (file-exists-p path))
      (message (format "Opening `%s'" path))
      (w32-browser path))))

;;;###autoload
(defun amstore-get-heading-names ()
  "Try to get names from a more complex headline."
  (interactive)
  (let ((hdg (nth 4 (org-heading-components)))
        h1
        h2)
    (string-match "^\\([^(]*\\)?[[:space:]]*(\\(.*\\))" hdg)
    (when (and (<= (match-beginning 1) (length hdg))
               (<= (match-beginning 2) (length hdg)))
      (setq h1 (match-string 1 hdg))
      (setq h2 (match-string 2 hdg))
      (or (> (length h1) 3)
          (setq h1 nil))
      (or (> (length h2) 3)
          (setq h2 nil))
      (cond
       ((or (conforming-part-number-p h1) (not h2))
        (string-trim h1))
       ((or (conforming-part-number-p h2) (not h1))
        (string-trim h2))))))

;;;###autoload
(defun amstore-get-part-runtime (part &optional arg)
  "Calculate the runtime for one PART from its STP file.
The display format can be changed by populating ARG."
  (interactive "sPart number: \nP")
  (let ((timeregex "TOTAL TIME\\ *:\\ *\\([0-9.]*\\) minutes\\ *\\([0-9.]*\\)")
        (countregex "Parts/Sheet:[ \t]*\\([0-9]*\\)")
        (qty)
        (minutes)
        (seconds)
        (per-part-time)
        (filepath (concat amstore--stp-path "/" part ".txt")))
    (with-temp-buffer
      (insert-file-contents filepath)
      (string-match countregex (buffer-string))
      (setq qty (string-to-number (match-string 1 (buffer-string))))
      (string-match timeregex (buffer-string))
      (setq minutes (string-to-number (match-string 1 (buffer-string))))
      (setq seconds (string-to-number (match-string 2 (buffer-string)))))
    (save-excursion
      (if (not (> qty 0))
          (error "Couldn't find qty or qty was 0")
        (setq per-part-time (/ (+ minutes (/ seconds 60)) qty))
        (if arg
            (progn
              (beginning-of-line)
              (kill-line)
              (insert
               (format "| %s | %f | %d |" part per-part-time qty)))
          (insert
           (format "%f - Qty: %d " per-part-time qty)))))))

;;;###autoload
(defun amstore-open-related-xls ()
  ""
  (interactive)
  (let ((xls-open (org-entry-get (point) "XLS" t nil))
        (headingtext (nth 4 (org-heading-components))))
    (unless xls-open
      (setq to-open (read-file-name
                     (format "Enter path of `%s': " headingtext)
                     amstore--mtl-path)))
    (when xls-open
      (if (not (file-exists-p xls-open))
          (error (format "Couldn't find `%s'" xls-open))
        (org-set-property "XLS" xls-open)
        (w32-browser xls-open)))))

;;;###autoload
(defun amstore-get-headline-part-runtime (&optional arg)
  "Calculate the runtime for one part with the same name as an org headline.
The display format can be changed by populating ARG."
  (interactive "P")
  (let ((part (org-get-heading t t t t)))
    (amstore-get-part-runtime part arg)))

;;;###autoload
(defun conforming-part-number-p (partnum)
  "Return t if PARTNUM is conforming."
  (let ((conforming-old-regexp "^\\([Zz][0-9]\\{5,6\\}\\)[[:space:]]?\\(.*\\)?")
        (conforming-new-regexp "^\\([A-Za-z]\\{3,4\\}\\)\\([0-9]\\{4\\}\\)-?.*"))
    (cond
     ((string-match conforming-new-regexp partnum)
      t)
     ((string-match conforming-old-regexp partnum)
      t)
     (t
      nil))))

;;;###autoload
(defun amstore-copy-job-number-to-clipboard ()
  "Copy the job number in this entry to the system clipboard."
  (interactive)
  (let ((entry (org-get-entry)))
    (string-match "Job Number: \\(.*\\)" entry)
    (if (not (setq match (match-string 1 entry)))
        (error "Couldn't find a job number")
      (w32-set-clipboard-data match)
      (message (format "Copied `%s' to w32 clipboard." match)))))

;;;###autoload
(defun amstore-copy-metal-path-to-clipboard ()
  "Copy the metal CAD path to w32 clipboard."
  (interactive)
  (w32-set-clipboard-data amstore--mtl-path)
  (message (format "Copied `%s' to w32 clipboard." amstore--mtl-path)))

;;;###autoload
(add-hook 'org-mode-hook 'amstore-mode)

(provide 'amstore-mode)
;;; amstore-mode.el ends here
