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
(require 'delsel)
(require 'alert)

(defvar amstore--stp-path "G:/STRIKER LASER PROGRAMS/STP"
  "Path to setup files for laser.")

(defvar amstore--mtl-path "S:\\shared\\general\\Metals\\METAL MANUFACTURING"
  "Path to metal department models and drawings.
This is here only because it's convienient to copy it to the w32 clipboard all the time.")

(defvar amstore--tea-alarm-sound "c:/users/juntunenkc/dropbox/BEEP2.WAV"
  "Path to an alarm sound.")

(defvar amstore--tea-timers nil
  "A list of tea timers we started.")

;;;###autoload
(define-minor-mode amstore-mode
  "A container for handy, Amstore-related functions."
  :lighter " ∀"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c b") 'amstore-org-headline-w32-browser)
            (define-key map (kbd "C-c d") 'amstore-open-drawing)
            (define-key map (kbd "C-c g") 'amstore-get-headline-part-runtime)
            (define-key map (kbd "C-c G") 'amstore-get-part-runtime)
            (define-key map (kbd "C-c j") 'amstore-copy-job-number-to-clipboard)
            (define-key map (kbd "C-c J") 'amstore-copy-item)
            (define-key map (kbd "C-c m") 'amstore-copy-metal-path-to-clipboard)
            (define-key map (kbd "C-c t") 'amstore-steep-tea)
            (define-key map (kbd "C-c x") 'amstore-open-related-xls)
            map))

(defun amstore--org-buffer-prop (prop)
  "Return the value of a buffer property named PROP.

There has to already be a function for this, but I couldn't find it."
  (if (string-match (concat "^#\\+\\(" prop "\\):[ \t]*\\(.*\\)") (buffer-string) 0)
      (match-string 2 (buffer-string))
    nil))

(defun amstore--sanitize-path (path &optional filename)
  "Make PATH all proper. If FILENAME is non-nil, output a sanitized filename."
  (cond
   (path
    (with-temp-buffer
      (insert (subst-char-in-string ?\\ ?/ path t))
      (let ((match (string-match "/$" (buffer-string))))
        (setq match (string-match "/$" (buffer-string)))
        (unless (or match filename)
          (goto-char (point-max))
          (insert "/")))
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
            (amstore--open to-open headingtext)
          (setq to-open (amstore--sanitize-path (read-file-name
                                                 (format "Enter path of `%s': " headingtext)
                                                 amstore--mtl-path) t))
          (if (not (file-exists-p to-open))
              (error (format "File `%s' doesn't exist!" to-open))
            (amstore--open to-open headingtext)))))))

(defun amstore--open (path &optional description)
  "Store PATH as a org-link, and fill the DESCRIPTION if there is one.
Otherwise, we'll just use the `file-name-base' for a description."
  (let* ((descr (if description description
                  (file-name-base path))))
    (org-set-property "MODEL" path)
    (org-set-property "SLDDRW" (format "[[file:%s][%s]]" path descr))
    (w32-browser path)
    (message (format "Opening '%s'..." path))))

;;;###autoload
(defun amstore-open-drawing ()
  "Open a PDF, if we have one."
  (interactive)
  (let ((to-open (amstore--get-link-path "Drawing")))
    (if (not to-open)
        (if (not to-open)
            (error "No drawing link found!")
          (error (format "File `%s' doesn't exist!" to-open)))
      (w32-browser to-open))))

(defun amstore--get-link-path (prop)
  "Look for a link under PROP and return the path."
  (let ((link (org-entry-get (point) prop)))
    (when link
      (string-match "\\[\\[file:\\(.*\\)\\]\\[\\(.*\\)\\]\\]" link)
      (with-temp-buffer
        (insert (match-string 1 link))
        (subst-char-in-string ?\\ ?/ (buffer-string))))))

;;;###autoload
(defun amstore-open-containing-dir ()
  "Open the path associated with this org heading."
  (interactive)
  (let* ((model (org-entry-get (point) "MODEL" t nil))
         (path (with-temp-buffer
                 (insert (amstore--sanitize-path model t))
                 (goto-char (point-max))
                 (set-mark (point-max))
                 (search-backward "/")
                 (forward-char)
                 (delete-active-region)
                 (buffer-string))))
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
  "Open an XLS[X] file, if we have one."
  (interactive)
  (let ((xls-open (org-entry-get (point) "XLS" t nil))
        (headingtext (nth 4 (org-heading-components))))
    (unless xls-open
      (setq xls-open (read-file-name
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
(defun amstore-copy-item (&optional arg)
  "Copy the pertinant headline bit to w32 clipboard.
If there's an ARG, copy the heading and the job number."
  (interactive "P")
  (let* ((headingtext (nth 4 (org-heading-components)))
         (match)
         (entry (org-get-entry))
         (to-copy headingtext))
    (when arg
      (string-match "Job Number: \\(.*\\)" entry)
      (when (setq match (match-string 1 entry))
        (setq to-copy (format "%s %s" match headingtext))))
    (w32-set-clipboard-data to-copy)
    (message (format "Copied `%s' to w32 clipboard." to-copy))))

;;;###autoload
(defun amstore-copy-job-number-to-clipboard ()
  "Copy the job number in this entry to the system clipboard."
  (interactive)
  (let ((entry (org-get-entry))
        (match))
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
(defun amstore-steep-tea (&optional min)
  "Steep tea for 3 minutes by default. An argument for MIN is a number of minutes."
  (interactive "P")
  (let ((minutes 3)
        (timeword "minute")
        (new-timer))
    (if (and min (integerp min) (> min 0))
        (setq minutes min))
    (if (> minutes 1)
        (setq timeword "minutes"))
    (message (format "Setting a timer for %d %s." minutes timeword))
    (setq new-timer
          (run-at-time (format "%d min" minutes) nil
                       #'amstore--alert amstore--tea-alarm-sound))
    (if amstore--tea-timers
        (push new-timer amstore--tea-timers)
      (setq amstore--tea-timers `(,new-timer)))))

(defun amstore--alert (sound &optional message)
  "Alert the user with SOUND and MESSAGE."
  (let ((msg (or message "Tea is ready!")))
    (play-sound-file sound)
    (alert msg :style 'mode-line)))

(defun amstore-cancel-timers ()
  "Cancel tea timer(s)."
  (interactive)
  (while amstore--tea-timers
    (cancel-timer (pop amstore--tea-timers))))

;;;###autoload
(add-hook 'org-mode-hook 'amstore-mode)

(provide 'amstore-mode)
;;; amstore-mode.el ends here
