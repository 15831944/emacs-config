;;; york-mode.el --- a minor mode to contain functions for work -*- lexical-binding: t -*-

;; Copyright (C) 2018 K. C. Juntunen

;; Author   : K. C. Juntunen <juntunen.kc@gmail.com>
;; URL      : https://github.com/kcjuntunen/emacs-config/blob/master/york-mode.el
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

(defvar york--tea-alarm-sound "c:/users/k.c.juntunen/dropbox/BEEP2.WAV"
  "Path to an alarm sound.")

(defvar york--tea-timers nil
  "A list of tea timers we started.")

;;;###autoload
(define-minor-mode york-mode
  "A container for handy, York-related functions."
  :lighter " ¥"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t") 'york-steep-tea)
            map))

(defun york--org-buffer-prop (prop)
  "Return the value of a buffer property named PROP.

There has to already be a function for this, but I couldn't find it."
  (if (string-match (concat "^#\\+\\(" prop "\\):[ \t]*\\(.*\\)") (buffer-string) 0)
      (match-string 2 (buffer-string))
    nil))

(defun york--sanitize-path (path &optional filename)
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
(defun york-open-containing-dir ()
  "Open the path associated with this org heading."
  (interactive)
  (let* ((model (org-entry-get (point) "MODEL" t nil))
         (path (with-temp-buffer
                 (insert (york--sanitize-path model t))
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
(defun york-steep-tea (&optional min)
  "Steep tea for 3 minutes by default. An argument for MIN is a number of minutes."
  (interactive "P")
  (let ((minutes 3)
        (timeword "minute")
        (new-timer)
        (start-time (elt (split-string (time-stamp-string) " ") 1)))
    (if (and min (integerp min) (> min 0))
        (setq minutes min))
    (if (> minutes 1)
        (setq timeword "minutes"))
    (message (format "Setting a timer for %d %s." minutes timeword))
    (setq new-timer
          (run-at-time (format "%d min" minutes) nil
                       #'york--alert york--tea-alarm-sound
                       (format "Tea started at %s is ready." start-time)))
    (if york--tea-timers
        (push new-timer york--tea-timers)
      (setq york--tea-timers `(,new-timer)))))

(defun york--alert (sound &optional message)
  "Alert the user with SOUND and MESSAGE."
  (let ((msg (or message "Tea is ready!")))
    (play-sound-file sound)
    (alert msg :style 'mode-line)))

(defun york-cancel-timers ()
  "Cancel tea timer(s)."
  (interactive)
  (while york--tea-timers
    (cancel-timer (pop york--tea-timers))))

;;;###autoload
(add-hook 'org-mode-hook 'york-mode)

(provide 'york-mode)
;;; york-mode.el ends here
