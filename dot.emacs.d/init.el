;;; init.el --- My init.el file.

;; Copyright (C) 2019 K. C. Juntunen

;; Author: K. C. Juntunen <juntunen.kc@gmail.com>
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; The idea is to replace the Spacemacs stuff I like, and start in less than 3 seconds.

;;; Code:
(defconst kc/emacs-start-time (current-time))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'tooltip-mode)
    (tooltip-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist (cons 'font
					(if (string-equal (window-system) "x")
					    "Anka/Coder Condensed:style=Regular"
					  "Consolas")))

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
;; (bind-key (kbd "C-x ;") 'comment-or-uncomment-region)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(use-package emacs
  :custom
  (blink-cursor-delay .2)
  (blink-cursor-interval .2)
  (blink-cursor-blinks 10000)
  (scroll-step 1)
  (sentence-end-double-space nil)
  (scroll-margin 0)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position 1)
  (show-paren-delay 0)
  (make-backup-files nil)
  (auto-save-default nil)
  (whitespace-line-column 80) ;; limit line length
  (whitespace-style '(face tabs empty trailing lines-tail))
  (inhibit-startup-screen t)
  :config
  (whitespace-mode 1)
  (show-paren-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  :hook
  (prog-mode . linum-mode))
(use-package which-key
  :diminish which-key
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(unless (version< emacs-version "24.4")
  (use-package magit))

(defun edit-init-file ()
  "Open `init.el' for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(use-package evil
  :config
  (evil-mode 1))

(use-package general
  :config
  (general-unbind
    :states '(normal visual emacs motion)
    "SPC")
  (general-unbind
    :states '(normal visual emacs motion)
    ",")

  (general-create-definer
    kc/mode-leader-keys
    :states '(emacs normal visual motion insert)
    :non-normal-prefix "C-,"
    :prefix ",")

  (general-create-definer
    kc/leader-keys
    :states '(emacs normal visual motion insert)
    :non-normal-prefix "C-SPC"
    :prefix "SPC")
  (kc/leader-keys
    "hdF" 'describe-face
    "hdk" 'describe-key
    "hdf" 'describe-function
    "hdv" 'describe-variable
    "hdc" 'describe-char
    "hdo" 'describe-symbol
    "hi"  '(info :which-key "Info")
    "h?"  '(describe-bindings :which-key "Describe bindings")
    "iu"  'insert-char
    "wl"  '(windmove-right :which-key "move right")
    "wh"  '(windmove-left :which-key "move left")
    "wk"  '(windmove-up :which-key "move up")
    "wj"  '(windmove-down :which-key "move bottom")
    "w/"  '(split-window-right :which-key "split right")
    "w-"  '(split-window-below :which-key "split bottom")
    "wx"  '(delete-window :which-key "delete window")
    "lv"  'visual-line-mode
    "lt"  'toggle-truncate-lines

    "bd"  'kill-this-buffer
    "fs"  '(save-buffer :which-key "Save this buffer")
    "fed" '(edit-init-file :which-key "Edit init file")

    "gs"  '(magit-status :which-key "Magit Status")
    "aoa" '(org-agenda-list :which-key "Org Agenda")
    "aoo" 'org-agenda
    "ac" '(calc-dispatch :which-key "Calc Dispatch")
    "ad"  'dired
    "'"   '(eshell :which-key "Eshell")
    "aP"  'proced
    
    "qq"  'save-buffers-kill-emacs)
  (kc/mode-leader-keys
    :keymaps 'prog-mode-map
    ";" 'comment-or-uncomment-region))

(use-package spacemacs-theme
  :init
  (load-theme 'spacemacs-dark t)
  :custom
  (spacemacs-theme-org-agenda-height nil)
  (spacemacs-theme-org-height nil))

(use-package spaceline
  :demand t
  :custom
  (powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package helm
  :diminish helm-mode
  :init
  (require 'helm-config)
  :custom
  (helm-M-x-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-locate-fuzzy-match t)
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-candidate-number-list 150)
  (helm-split-window-in-side-p t)
  (helm-move-to-line-cycle-in-source t)
  (helm-autoresize-max-height 0)
  (helm-autoresize-min-height 20)
  :config
  (helm-mode 1)
  :general
  ("M-x" 'helm-M-x
   "C-x C-f" 'helm-find-file)
  (kc/leader-keys
    "TAB" '(switch-to-prev-buffer :which-key "find files")
    "ff"  'helm-find-files
    "fl"  'helm-locate
    "SPC" 'helm-M-x
    "bb"  'helm-buffers-list))

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-org-rifle
  :general
  (kc/leader-keys
    "aor" 'helm-org-rifle))

(use-package avy
  :general
  (kc/leader-keys
    ":" 'avy-goto-char))

(use-package anzu
  :config
  (global-anzu-mode 1))

(use-package flycheck
  :hook
   (prog-mode . flycheck-mode))
  
;; (use-package counsel
;;   :ensure t
;;   :general
;;   ("M-x" 'counsel-M-x
;;    "C-x C-f" 'counsel-find-file)
;;   (kc/leader-keys
;;     "TAB" '(switch-to-prev-buffer :which-key "find files")
;;     "ff"  'counsel-find-file
;;     "fj"  'counsel-file-jump
;;     "fl"  'counsel-locate
;;     "hdF" 'counsel-describe-face
;;     "hdb" 'counsel-descbinds
;;     "hdf" 'counsel-describe-function
;;     "hdv" 'counsel-describe-variable
;;     "hi"  '(info :which-key "Info")
;;     "h?"   '(describe-bindings :which-key "Describe bindings")
;;     "iu"  'counsel-unicode-char
;;     "sr"  'counsel-rg
;;     "ss"  'counsel-grep-or-swiper
;;     "wl"  '(windmove-right :which-key "move right")
;;     "wh"  '(windmove-left :which-key "move left")
;;     "wk"  '(windmove-up :which-key "move up")
;;     "wj"  '(windmove-down :which-key "move bottom")
;;     "w/"  '(split-window-right :which-key "split right")
;;     "w-"  '(split-window-below :which-key "split bottom")
;;     "wx"  '(delete-window :which-key "delete window")
;;     "y"   'counsel-yank-pop
;;     "SPC" 'counsel-M-x
;;     "bb"  'counsel-switch-buffer
;;     "bd"  'kill-this-buffer
;;     "fs" '(save-buffer :which-key "Save this buffer")
;;     "fed" '(edit-init-file :which-key "Edit init file")

;;     "gs" '(magit-status :which-key "Magit Status")
;;     "aoa" '(org-agenda-list :which-key "Org Agenda")
;;     "aoc" '(calc-dispatch :which-key "Org Agenda")
;;     "'" '(eshell :which-key "Eshell")
;;     "qq"  'save-buffers-kill-emacs)
;;   :commands counsel-describe-face)

;; (general-define-key
;; 	   :states '(normal visual insert emacs)
;; 	   :prefix "SPC"
;; 	   :non-normal-prefix "M-SPC"
;; 	   "TAB" '(switch-to-prev-buffer :which-key "find files")
;; 	   "SPC" '(helm-M-x :which-key "Emacs Commands")
;; 	   "SPC-u" '(universal-argument :which-key "Universal argument")
;; 	   "bb" '(helm-mini :which-key "buffers list")
;; 	   "bd" '(kill-this-buffer :which-key "delete buffer")

;; 	   "wl" '(windmove-right :which-key "move right")
;; 	   "wh" '(windmove-left :which-key "move left")
;; 	   "wk" '(windmove-up :which-key "move up")
;; 	   "wj" '(windmove-down :which-key "move bottom")
;; 	   "w/" '(split-window-right :which-key "split right")
;; 	   "w-" '(split-window-below :which-key "split bottom")
;; 	   "wx" '(delete-window :which-key "delete window")

;; 	   "hi" '(info :which-key "Info")
;; 	   "hdv" '(describe-variable :which-key "Describe variable")
;; 	   "hdf" '(describe-function :which-key "Describe function")
;; 	   "hdk" '(describe-key :which-key "Describe key")
;; 	   "?" '(describe-bindings :which-key "Describe bindings")

;; 	   "ff" '(helm-find-files :which-key "Find Files")
;; 	   "fs" '(save-buffer :which-key "Save this buffer")
;; 	   "fed" '(edit-init-file :which-key "Edit init file")

;; 	   "gs" '(magit-status :which-key "Magit Status")
;; 	   "aoa" '(org-agenda :which-key "Org Agenda")
;; 	   "am" '(eshell :which-key "Eshell")

;; 	   ;; "at" '(ansi-term :which-key "open terminal")
;; 	   "qq" '(save-buffers-kill-emacs :which-key "Kill Emacs"))
;; (general-define-key :keymap universal-argument-map
;; 		      :states '(normal visual)
;; 		      :prefix "SPC"
;; 		      "u" 'universal-argument-more))


(use-package company
  :diminish company
  :init
  :hook
  (prog-mode . company-mode)
  (after-init . global-company-mode)
  :config
  (use-package company-irony)
  (use-package company-anaconda)
  :custom
  (company-idle-delay              0)
  (company-minimum-prefix-length   2)
  (company-show-numbers            t)
  (company-tooltip-limit           20)
  (company-dabbrev-downcase        nil)
  (company-tooltip-flip-when-above t)
  (company-backends                '((company-anaconda
                                      company-bbdb
                                      company-elisp
                                      company-gtags
                                      company-irony
                                      company-shell))))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (sp-use-paredit-bindings))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package evil-org
  :init
  (require 'evil-org)
  (require 'evil-org-agenda)
  :hook
  (org-mode . evil-org-mode))

(use-package org-variable-pitch
  :hook
  (org-mode . org-variable-pitch-minor-mode)
  :custom
  (org-variable-pitch-fixed-font "Consolas")
  :config
  (set-face-attribute 'org-variable-pitch-face nil :family "Consolas")
  (set-face-attribute 'variable-pitch nil :family "Segoe UI"))

(use-package org
  :custom
  (org-directory (if (string-equal window-system "w32")
			  "~/../../org"
			"~/Dropbox/org"))
  (org-agenda-span 'day)
  (kc/agenda-dir (concat org-directory ""))
  (org-agenda-file-regexp "\\`[^.].*\\.org\\'")
  (org-use-fast-todo-selection t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-ellipsis "⤵")
  (kc/org-all-agenda-files (directory-files
			    (expand-file-name kc/agenda-dir) t org-agenda-file-regexp))
  (org-refile-targets (quote ((nil :maxlevel . 1) (kc/org-all-agenda-files :maxlevel . 2))))
  (org-catch-invisible-edits 'smart)
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :fileskip0 t :formula %
                                                  :properties ("RequestNbr" "Billable" "SectionNbr" "TaskNbr")))
  (org-deadline-warning-days 45)
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-columns-default-format "%25ITEM(Task) %40Description %20Captured %10Effort(Effort){:} %10CLOCKSUM")
  (org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit"))))
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "WIP(n)" "|" "DONE(d)" "CANCELLED(c/!)")
	   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c/!)" "PHONE" "MEETING"))))
  (org-todo-keyword-faces
   (quote (("TODO" :foreground "red" :weight bold)
	   ("WIP" :foreground "blue" :weight bold)
	   ("DONE" :foreground "forest green" :weight bold)
	   ("WAITING" :foreground "orange" :weight bold)
	   ("HOLD" :foreground "magenta" :weight bold)
	   ("CANCELLED" :foreground "forest green" :weight bold)
	   ("MEETING" :foreground "forest green" :weight bold)
	   ("PHONE" :foreground "forest green" :weight bold))))
  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("ARCHIVE" . t))
	   ("WAITING" ("WAITING" . t))
	   ("HOLD" ("WAITING") ("HOLD" . t))
	   (done ("WAITING") ("HOLD"))
	   ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	   ("WIP" ("WAITING") ("CANCELLED") ("HOLD"))
	   ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  (kc/refile-file (concat kc/agenda-dir "/refile.org"))
  (kc/diary-file (concat org-directory "/diary.org"))
  (kc/notes-file (concat org-directory "/notes.org"))
  (org-capture-templates
	'(("t" "todo" entry
           (file kc/refile-file)
           "* TODO %?
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %a
  :END:" :clock-in t :clock-resume t)
          ("p" "Phone call" entry
           (file kc/refile-file)
           "* PHONE %?
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %a
  :END:" :clock-in t :clock-resume t)
          ("j" "Journal" entry
           (file+olp+datetree kc/diary-file)
           "* %?
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %a
  :END:" :clock-in t :clock-resume t)
          ("n" "Note" entry
           (file kc/notes-file)
           "* %? :NOTE:
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %a
  :END:" :clock-in t :clock-resume t)
          ("m" "Meeting" entry
           (file kc/notes-file)
           "* MEETING %?
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %a
  :END:" :clock-in t :clock-resume t)))
  :general
  (kc/mode-leader-keys
    :keymap 'org-mode-map
    "P" 'org-set-property
    "tic" 'org-table-insert-column
    "tir" 'org-table-insert-row
    "ti-" 'org-table-insert-hline
    "tdc" 'org-table-delete-column)
  (kc/leader-keys
    :keymap 'org-agenda-mode-map
    "TAB" '(switch-to-prev-buffer :which-key "find files")
    "ff"  'helm-find-files
    "fl"  'helm-locate
    "SPC" 'helm-M-x
    "bd"  'kill-this-buffer
    "bb"  'helm-buffers-list)
  (kc/mode-leader-keys
    :keymap 'org-agenda-mode-map
    "l" 'org-agenda-log-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))

(defun kc/copy-query-notes ()
  "Copy a query string to the clipboard for the `notes' table for the last hour."
  (interactive)
  (w32-set-clipboard-data
   (format "SELECT * FROM notes WHERE NoteDateTime > '%s' AND Tag1 LIKE '%%ATHOS%%'"
	   (format-time-string "%Y-%m-%d %H:%M:%S"
			       (time-subtract (current-time)
					      (seconds-to-time (* 60 60 30))) "wall"))))

(defun kc/copy-query-incidents ()
  "Copy a query string to the clipboard for the `incidents' table for the last hour."
  (interactive)
  (w32-set-clipboard-data
   (format "SELECT * FROM incidents WHERE DateAdded > '%s' AND Resolved <> 'Y'"
	   (format-time-string "%Y-%m-%d %H:%M:%S"
			       (time-subtract (current-time)
					      (seconds-to-time (* 60 60 30))) "wall"))))
(kc/leader-keys
  "cqn" 'kc/copy-query-notes
  "cqi" 'kc/copy-query-incidents)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("c:/Users/K.C.Juntunen/OneDrive/org/superior.org" "~/../../OneDrive/org/work.org" "~/../../OneDrive/org/theisen.org" "~/../../OneDrive/org/plantation.org" "~/../../OneDrive/org/palko.org" "~/../../OneDrive/org/orsheln.org" "~/../../OneDrive/org/notes.org" "~/../../OneDrive/org/north40.org" "~/../../OneDrive/org/mfwestern.org" "~/../../OneDrive/org/kelsan.org" "~/../../OneDrive/org/jci.org" "~/../../OneDrive/org/clarion.org" "~/../../OneDrive/org/berne.org")))
 '(package-selected-packages
   (quote
    (csharp-mode csv-mode company-jedi company-anaconda pretty-symbols magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red")))))

(load-file "~/.personal.el")
(defconst kc/after-init (current-time))
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            kc/emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         kc/emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
	    t))

(provide 'init)
;;; init.el ends here
