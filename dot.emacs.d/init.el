(defconst kc/emacs-start-time (current-time))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq sentence-end-double-space nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(defmacro kc/start-time (name)
  `(defvar ,(intern (concat "d/time-" name)) (current-time)))

(defmacro kc/end-time (name)
  `(message "`%s' execution took %.5f seconds."
            ,name
	    (float-time (time-subtract (current-time) ,(intern (concat "d/time-" name))))))

(add-to-list 'default-frame-alist '(font . "Anka/Coder Condensed:style=Regular"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
;; (package-initialize)
(bind-key (kbd "C-x ;") 'comment-or-uncomment-region)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :init
;;   (require 'helm-config)
;;   (setq helm-M-x-fuzzy-match t
;; 	helm-buffers-fuzzy-matching t
;; 	helm-recentf-fuzzy-match t
;; 	helm-locate-fuzzy-match t
;; 	helm-semantic-fuzzy-match t
;; 	helm-imenu-fuzzy-match t
;; 	helm-completion-in-region-fuzzy-match t
;; 	helm-candidate-number-list 150
;; 	helm-split-window-in-side-p t
;; 	helm-move-to-line-cycle-in-source t
;; 	helm-autoresize-max-height 0
;; 	helm-autoresize-min-height 20)
;;   :config
;;   (helm-mode 1)
;;    :bind (("M-x"   . helm-M-x)
;; 	   ("C-x C-m" . helm-M-x)
;; 	   ("M-y"     . helm-show-kill-ring)
;; 	   ("C-x b"   . helm-mini)
;; 	   ("C-x C-f" . helm-find-files)
;; 	   ("C-h f"   . helm-apropos)
;; 	   ("C-h r"   . helm-info-emacs)
;; 	   ("C-h C-l" . helm-locate-library)
;; 	   :map helm-command-map
;; 	   ("o"       . helm-occur)
;; 	   ("g"       . helm-do-grep)
;; 	   ("C-c w"   . helm-wikipedia-suggest)
;; 	   ("SPC"     . helm-all-mark-rings)))

;; (use-package helm-descbinds
;;   :ensure t
;;   :defer t
;;   :bind (("C-h b" . helm-descbinds)))

(use-package which-key
  :ensure t
  :diminish which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(unless (version< emacs-version "24.4")
  (use-package magit
    :ensure t
    :defer t))

(defun edit-init-file ()
  "Open `init.el' for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package general
  :ensure t
  :config
  (general-unbind
  :states '(normal visual emacs motion)
  "SPC")

  (general-create-definer
    kc/mode-leader-keys
    :states '(emacs normal visual motion insert)
    :non-normal-prefix "C-,"
    :prefix ",")

  (general-create-definer
    kc/leader-keys
    :states '(emacs normal visual motion insert)
    :non-normal-prefix "C-SPC"
    :prefix "SPC"))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  (setq spacemacs-theme-org-agenda-height nil
	spacemacs-theme-org-height nil))

(use-package spaceline
  :ensure t
  :demand t
  :defer t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package counsel
  :ensure t
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file)
  (kc/leader-keys
    "TAB" '(switch-to-prev-buffer :which-key "find files")
    "ff"  'counsel-find-file
    "fj"  'counsel-file-jump
    "fl"  'counsel-locate
    "hdF" 'counsel-describe-face
    "hdb" 'counsel-descbinds
    "hdf" 'counsel-describe-function
    "hdv" 'counsel-describe-variable
    "hi"  '(info :which-key "Info")
    "h?"   '(describe-bindings :which-key "Describe bindings") 
    "iu"  'counsel-unicode-char
    "sr"  'counsel-rg
    "ss"  'counsel-grep-or-swiper
    "wl"  '(windmove-right :which-key "move right")
    "wh"  '(windmove-left :which-key "move left")
    "wk"  '(windmove-up :which-key "move up")
    "wj"  '(windmove-down :which-key "move bottom")
    "w/"  '(split-window-right :which-key "split right")
    "w-"  '(split-window-below :which-key "split bottom")
    "wx"  '(delete-window :which-key "delete window")
    "y"   'counsel-yank-pop
    "SPC" 'counsel-M-x
    "bb"  'counsel-switch-buffer
    "bd"  'kill-this-buffer
    "fs" '(save-buffer :which-key "Save this buffer")
    "fed" '(edit-init-file :which-key "Edit init file")

    "gs" '(magit-status :which-key "Magit Status")
    "aoa" '(org-agenda-list :which-key "Org Agenda")
    "aoc" '(calc-dispatch :which-key "Org Agenda")
    "'" '(eshell :which-key "Eshell")
    "qq"  'save-buffers-kill-emacs)
  :commands counsel-describe-face)

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

(setq show-paren-delay 0)
(show-paren-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

(use-package company-anaconda
  :ensure t
  :defer t)

(use-package company-shell
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              0
        company-minimum-prefix-length   2
        company-show-numbers            t
        company-tooltip-limit           20
        company-dabbrev-downcase        nil
        company-tooltip-flip-when-above t
        company-backends                '((company-anaconda
                                           company-bbdb
                                           company-elisp
                                           company-gtags
                                           company-irony
                                           company-shell))))

(use-package smartparens
  :ensure t
  :config (smartparens-global-mode 1)
  (sp-use-paredit-bindings))

(whitespace-mode 1)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq dropbox-dir "~/Dropbox/")
(setq org-agenda-span 'day
      org-directory (concat dropbox-dir "/org")
      kc/agenda-dir (concat org-directory "")
      org-agenda-file-regexp "\\`[^.].*\\.org\\'"
      org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-ellipsis "⤵"
      kc/org-all-agenda-files (directory-files
                               (expand-file-name kc/agenda-dir) t org-agenda-file-regexp)
      org-refile-targets (quote ((nil :maxlevel . 1) (kc/org-all-agenda-files :maxlevel . 2)))
      org-catch-invisible-edits 'smart
      org-agenda-clockreport-parameter-plist '(:link t :maxlevel 1 :fileskip0 t)
      org-deadline-warning-days 45
      org-agenda-window-setup 'current-window
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-log-mode-items '(closed)
      org-columns-default-format "%25ITEM(Task) %40Description %20Captured %10Effort(Effort){:} %10CLOCKSUM"
      org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit")))
      org-todo-keywords
      (quote ((sequence "TODO(t)" "WIP(n)" "|" "DONE(d)" "CANCELLED(c/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c/!)" "PHONE" "MEETING")))
      org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("WIP" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold)))
      org-todo-state-tags-triggers
      (quote (("CANCELLED" ("ARCHIVE" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("WIP" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
      kc/refile-file (concat kc/agenda-dir "/refile.org")
      kc/diary-file (concat org-directory "/diary.org")
      kc/notes-file (concat org-directory "/notes.org")
      org-capture-templates
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-agenda-files
   '("~/Dropbox/org/berne.org" "~/Dropbox/org/plantation.org" "~/Dropbox/org/palko.org" "~/Dropbox/org/oversight.org" "~/Dropbox/org/orsheln.org" "~/Dropbox/org/north40.org" "~/Dropbox/org/mfwestern.org" "~/Dropbox/org/kelsan.org" "~/Dropbox/org/theisen.org"))
 '(package-selected-packages '(magit use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
