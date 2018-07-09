
;;; amstore-mode.el --- a minor mode to contain functions for work -*- coding: utf-8-unix -*-

;; Copyright (C) 2018 K. C. Juntunen

;; Author   : K. C. Juntunen <juntunen.kc@gmail.com>
;; URL      :
;; Package-Version:
;; Version  : 0.1
;; Keywords :

;; This file is NOT part of GNU Emacs.

;;; Commentary:


;;; Code:

(defvar stp-path "G:/STRIKER LASER PROGRAMS/STP"
	"Path to setup files for laser.")

;;;###autoload
(define-minor-mode amstore-mode
	"A container for handy, Amstore-related functions."
	:lighter " ∀"
	:keymap (let ((map (make-sparse-keymap)))
						(define-key map (kbd "C-c b") 'amstore/org-headline-w32-browser)
						(define-key map (kbd "C-c g") 'amstore/get-headline-part-runtime)
						(define-key map (kbd "C-c G") 'amstore/get-part-runtime)
						map))

(defun amstore/org-buffer-prop (prop)
	"Return the value of a buffer PROPerty."
	(if (string-match (concat "^#\\+\\(" prop "\\):[ \t]*\\(.*\\)") (buffer-string) 0)
			(match-string 2 (buffer-string))
		nil))

;; (defun amstore/org-headline-w32-browser (&optional arg)
;;   "`C-c b' Open SLDDRW
;;   `C-u C-c b' Open SLDPRT
;;   `C-u C-u C-c b' Open SLDASM"
;;   (interactive "P")
;;   (let* ((path (or (org-entry-get (point) "MDLPATH" t nil)
;;                    (amstore/org-buffer-prop "MDLPATH")))
;;          (ext "SLDDRW")
;;          headingtext
;; 				 hdg
;; 				 althdg
;; 				 to-open
;; 				 (prop "MDLPATH"))
;; 		(if (and (setq to-open (org-entry-get (point) "MODEL" nil nil))
;; 						 (file-exists-p to-open))
;; 				(progn
;; 					(setq prop "MODEL")
;; 					(w32-browser to-open))

;; 			(case arg
;; 				(4 (setq ext "SLDPRT"))
;; 				(8 (setq ext "SLDASM"))
;; 				(16 (setq ext (org-entry-get (point) "EXT" nil))))
;; 			(setq headingtext (nth 4 (org-heading-components)))
;; 			;; (string-match "\\([Zz][0-9]\\{5,6\\}\\|[A-Za-z]\\{3,4\\}[0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)*\\)" headingtext)
;; 			(string-match "^.*\ " headingtext)
;; 			(setq hdg (match-string 1 headingtext))
;; 			(setq to-open (concat path "\\" hdg "." ext))
;; 			(if (file-exists-p to-open)
;; 					(progn
;; 						(org-set-property "MODEL" to-open)
;; 						(w32-browser to-open))
;; 				(string-match "(\\(.*\\))" headingtext)
;; 				(setq althdg (match-string 1 headingtext))
;; 				(setq to-open (concat path "\\" althdg "." ext))
;; 				(if (file-exists-p to-open)
;; 						(progn
;; 							(org-set-property "MODEL" to-open)
;; 							(w32-browser to-open)))
;; 				))))

(defun amstore/org-headline-w32-browser (&optional arg)
	""
	(interactive "P")
	(let* ((to-open (org-entry-get (point) "MODEL" t nil)))
		(if (and to-open (file-exists-p to-open))
				(w32-browser to-open)
			(let ((path (or (org-entry-get (point) "MDLPATH" t nil)
											(amstore/org-buffer-prop "MDLPATH")))
						(exts '("SLDDRW" "SLDASM" "SLDPRT"))
						(headingtext (nth 4 (org-heading-components))))
				(if (and path headingtext)
						(while (not (file-exists-p to-open))
							(setq to-open (concat path "\\" headingtext "." (car exts))
										exts (cdr exts))))
				(if (and to-open (file-exists-p to-open))
						(progn
							(org-set-property "MODEL" to-open)
							(w32-browser-open to-open))
					;; prompt for path here.
					)))))

(defun amstore/get-heading-names ()
	""
	(interactive)
	(setq hdg (nth 4 (org-heading-components)))
	;; (string-match "\\([Zz][0-9]\\{5,6\\}\\|[A-Za-z]\\{3,4\\}[0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)*\\)" hdg)
	(string-match "\\(^.*\\)[[:space:]]" hdg)
	(setq h1 (match-string 1 hdg))
	(string-match "(\\(.*\\))" hdg)
	(setq h2 (match-string 1 hdg))
	(format "hdg: `%s' 1: `%s', 2: `%s'" hdg h1 h2))

(defun amstore/maybe-remove-property (prop)
	""
	(if (and (not (eq prop nil)) (yes-or-no-p (format "Remove \"%s\" from properties? " prop)))
			(org-delete-property prop)
		nil))

(defun amstore/get-runtime-each (minutes seconds qty)
	"Convert MINUTES, SECONDS, and QTY to minutes of runtime on each part."
	(interactive "nMinutes: \nnSeconds: \nnQty: ")
	(insert
	 (format "%f"
					 (/ (+ (float minutes) (/ (float seconds) 60.0)) qty))))
(defvar stp-path "G:/STRIKER LASER PROGRAMS/STP"
	"Path to setup files for laser.")

(defun amstore/get-part-runtime (part &optional arg)
	"Get PART.STP and calculate runtime."
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

(defun amstore/get-headline-part-runtime (&optional arg)
	""
	(interactive "P")
	(let ((part (org-get-heading t t t t)))
		(amstore/get-part-runtime part arg)))

;;;###autoload
(add-hook 'org-mode-hook 'amstore-mode)

(provide 'amstore-mode)
