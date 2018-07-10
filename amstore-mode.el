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
(require 'evil)
(require 'subr-x)
(require 'w32-browser)

(defvar stp-path "G:/STRIKER LASER PROGRAMS/STP"
  "Path to setup files for laser.")

;;;###autoload
(define-minor-mode amstore-mode
  "A container for handy, Amstore-related functions."
  :lighter " ∀"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c b") 'amstore-org-headline-w32-browser)
            (define-key map (kbd "C-c g") 'amstore-get-headline-part-runtime)
            (define-key map (kbd "C-c G") 'amstore-get-part-runtime)
            (define-key map (kbd "C-c j") 'amstore-copy-job-number-to-clipboard)
            map))

(defun amstore--org-buffer-prop (prop)
  "Return the value of a buffer property named PROP.

There has to already be a function for this, but I couldn't find it."
  (if (string-match (concat "^#\\+\\(" prop "\\):[ \t]*\\(.*\\)") (buffer-string) 0)
      (match-string 2 (buffer-string))
    nil))

;;;###autoload
(defun amstore-org-headline-w32-browser ()
  "Look first in the `PROPERTIES' drawer for a path under `MODEL'.
Failing that, look in whatever path we can inherit, concatenate with the
heading, and try a few extensions. Failing that, ask for a filename."
  (interactive)
  (let ((to-open (org-entry-get (point) "MODEL" t nil)))
    (if (and to-open (file-exists-p to-open))
        (w32-browser to-open)
      (let ((path (or (org-entry-get (point) "MDLPATH" t nil)
                      (amstore--org-buffer-prop "MDLPATH")))
            (exts '("SLDDRW" "SLDASM" "SLDPRT" "PDF"))
            (headingtext (cond ((string-equal (org-entry-get (point) "Type") "Request")
                                (amstore-get-heading-names))
                               (t (nth 4 (org-heading-components))))))
        (if (and path headingtext)
            (while (and exts (not (and to-open (file-exists-p to-open))))
              (setq to-open (concat path headingtext "." (car exts))
                    exts (cdr exts))))
        (if (and to-open (file-exists-p to-open))
            (progn
              (org-set-property "MODEL" to-open)
              (w32-browser to-open))
          (setq to-open (read-file-name (format "Enter path of `%s': " headingtext)))
          (if (not (file-exists-p to-open))
              (error (format "File `%s' doesn't exist!" to-open))
            (org-set-property "MODEL" to-open)
            (w32-browser to-open)))))))

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
      (string-trim (or h1 h2)))))

(defvar stp-path "G:/STRIKER LASER PROGRAMS/STP"
  "Path to setup files for laser.")

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
        (filepath (concat stp-path "/" part ".txt")))
    (with-temp-buffer
      (insert-file-contents filepath)
      (string-match countregex (buffer-string))
      (setq qty (string-to-number (match-string 1 (buffer-string))))
      (string-match timeregex (buffer-string))
      (setq minutes (string-to-number (match-string 1 (buffer-string))))
      (setq seconds (string-to-number (match-string 2 (buffer-string)))))
    (save-excursion
      (if (not (> qty 0))
          (error "I'm retarded!")
        (setq per-part-time (/ (+ minutes (/ seconds 60)) qty))
        (if arg
            (progn
              (evil-delete-line (line-beginning-position) (1- (line-end-position)))
              (insert
               (format "| %s | %f | %d |" part per-part-time qty)))
          (insert
           (format "%f - Qty: %d " per-part-time qty)))))))

;;;###autoload
(defun amstore-get-headline-part-runtime (&optional arg)
  "Calculate the runtime for one part with the same name as an org headline.
The display format can be changed by populating ARG."
  (interactive "P")
  (let ((part (org-get-heading t t t t)))
    (amstore-get-part-runtime part arg)))

;;;###autoload
(defun amstore-copy-job-number-to-clipboard ()
  "Copy the job number in this entry to the system clipboard."
  (interactive)
  (let ((entry (org-get-entry)))
         (string-match "- Job Number: \\(.*\\)" entry)
         (if (setq match (match-string 1 entry))
             (w32-set-clipboard-data match)
           (error "Couldn't find a job number."))))

;;;###autoload
(add-hook 'org-mode-hook 'amstore-mode)

(provide 'amstore-mode)
;;; amstore-mode.el ends here
