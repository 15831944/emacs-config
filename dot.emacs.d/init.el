;; init.el --- My init.el file. -*- lexical-binding: t -*-

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
;; The idea is to replace the Spacemacs stuff I like, and start in less than 5
;; seconds.

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

(defvar not-win (eq system-type 'gnu/linux)
  "If NOT-WIN is non-nil, then we're not in MS-Windows.")

(defvar kc/fixed-width-font "Anka/Coder Condensed-10:style=Regular" "Default monospace font.")
(defvar kc/variable-pitch-font (if not-win
                                   "IBM Plex Serif Light"
                                 "Segoe UI") "Default variable pitch font.")

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist (cons 'font
                                        kc/fixed-width-font))

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(use-package emacs
  :defines
  personal-file
  show-paren-delay
  whitespace-line-column
  whitespace-style
  :init
  (add-hook 'after-init-hook '(lambda () (load-file custom-file)))
  (add-hook 'after-init-hook '(lambda () (load-file personal-file)))
  :config
  (setq-default blink-cursor-delay .2
                blink-cursor-interval .2
                blink-cursor-blinks 10000
                indent-tabs-mode nil
                scroll-step 1
                sentence-end-double-space nil
                scroll-margin 0
                scroll-conservatively 100000
                scroll-preserve-screen-position 1
                show-paren-delay 0
                make-backup-files nil
                auto-save-default nil
                whitespace-line-column 80 ;; limit line length
                whitespace-style '(face tabs empty trailing lines-tail)
                custom-file (if not-win
                                (expand-file-name "~/.emacs.d/custom.el")
                              (expand-file-name "~/../../OneDrive/custom.el"))
                personal-file (expand-file-name "~/.emacs.d/personal.el")
                initial-major-mode 'org-mode
                initial-scratch-message
                "#+title: Scratch buffer\n\n"
                inhibit-startup-screen t)
  (whitespace-mode 1)
  (show-paren-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (load-theme 'spacemacs-dark t)
  :hook
  (kill-emacs . (lambda () (persistent-scratch-save)))
  (prog-mode . company-mode)
  (prog-mode . linum-mode)
  (prog-mode . whitespace-mode))

(use-package persistent-scratch
  :config
  (persistent-scratch-autosave-mode t))

(use-package auctex
  :config
  (setq TeX-auto-save t
  TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package projectile
  :diminish (projectile-mode . "Πρ")
  :if not-win
  :after diminish
  :config
  (add-hook 'prog-mode-hook 'projectile-mode))

(use-package which-key
  :defer nil
  :functions
  which-key-mode
  :diminish which-key-mode
  :config
  (setq-default which-key-separator " "
                which-key-prefix-prefix "+")
  (which-key-mode 1))

(use-package diminish
  :defer nil)

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package eldoc
  :diminish eldoc-mode)

(use-package magit
  :if (not (version< emacs-version "24.4")))

(defun edit-init-file ()
  "Open `init.el' for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(use-package evil
  :config
  (evil-mode 1)
  (setq-default evil-respect-visual-line-mode t)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t))

(use-package evil-collection
  :defines
  evil-want-binding
  :after evil
  :init
  (setq-default evil-want-binding nil)
  :config
  (evil-collection-init))

(use-package helpful)

(use-package general
  :functions
  general-define-key
  :init
  (general-unbind
    :states '(normal visual emacs motion)
    "SPC")
  (general-unbind
    :states '(normal visual emacs motion)
    ",")
  :config
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
    "H"   '(hyperbole :which-key "∞ Hyperbole")
    "hdF" 'describe-face
    "hdk" 'helpful-key
    "hdf" 'helpful-function
    "hdv" 'helpful-variable
    "hdc" 'describe-char
    "hdo" 'helpful-symbol
    "hdm" 'describe-mode
    "hd?" '(describe-bindings :which-key "Describe bindings")
    "hi"  '(info :which-key "Info")
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

    "t="  'text-scale-increase
    "t-"  'text-scale-decrease

    "bd"  'kill-this-buffer
    "fs"  '(save-buffer :which-key "💾 Save this buffer")
    "fS"  '(save-some-buffers :which-key "🖴 Save all buffers")
    "fed" '(edit-init-file :which-key "Edit init file")

    "gs"  '(magit-status :which-key "Magit Status")
    "aoa" '(org-agenda-list :which-key "📅 Org Agenda List")
    "aoo" 'org-agenda
    "aoc" 'org-capture
    "ac"  '(calc-dispatch :which-key "🖩 Calc Dispatch")
    "ad"  'dired
    "ag"  '(gnus :which-key "🐮 Gnus")
    "'"   '(eshell :which-key "Eshell")
    "aP"  'proced

    "qq"  'save-buffers-kill-emacs)
  (kc/mode-leader-keys
    :keymaps 'prog-mode-map
    ";" 'comment-or-uncomment-region)
  (kc/mode-leader-keys
    :keymaps 'global-map
    "=" 'text-scale-increase
    "-" 'text-scale-decrease))

(use-package recentf
  :after general
  :config
  (recentf-mode 1))

;; (use-package spacemacs-theme
;;   :defines
;;   spacemacs-theme-org-agenda-height
;;   spacemacs-theme-org-height
;;   :init
;;   (load-theme 'spacemacs-light t)
;;   :config
;;   (setq-default spacemacs-theme-org-agenda-height nil
;;                 spacemacs-theme-org-height nil))

(use-package spaceline
  :demand t
  :config
  (setq-default powerline-default-separator 'arrow-fade)
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(use-package helm
  :diminish helm-mode
  :defines
  helm-M-x-fuzzy-match
  helm-buffers-fuzzy-matching
  helm-recentf-fuzzy-match
  helm-locate-fuzzy-match
  helm-semantic-fuzzy-match
  helm-imenu-fuzzy-match
  helm-completion-in-region-fuzzy-match
  helm-candidate-number-list
  helm-split-window-inside-p
  helm-move-to-line-cycle-in-source
  helm-autoresize-max-height
  helm-autoresize-min-height
  :init
  (require 'helm-config)
  :config
  (setq-default helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-locate-fuzzy-match t
                helm-semantic-fuzzy-match t
                helm-imenu-fuzzy-match t
                helm-completion-in-region-fuzzy-match t
                helm-candidate-number-list 150
                helm-split-window-inside-p t
                helm-move-to-line-cycle-in-source t
                helm-autoresize-max-height 0
                helm-autoresize-min-height 20)
  (helm-mode 1)
  :general
  ("M-x" 'helm-M-x
   "C-x C-f" 'helm-find-files)
  (kc/leader-keys
    "TAB" '(switch-to-prev-buffer :which-key "find files")
    "ff"  'helm-find-files
    "fr"  'helm-recentf
    "fl"  'helm-locate
    "SPC" 'helm-M-x
    "ho"  'helm-occur
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

(when not-win
  (use-package flycheck
    :diminish flycheck-mode
    :hook
    (prog-mode . flycheck-mode))

  (use-package yasnippet
    :diminish yas-minor-mode
    :if not-win
    :hook
    (prog-mode . yas-minor-mode))

  (use-package lsp-mode
    :if not-win
    :hook
    (c++-mode . lsp)
    (python-mode . lsp)
    :commands lsp)

  ;; optionally
  (use-package lsp-ui
    :commands lsp-ui-mode
    :config
    (setq lsp-ui-sideline-ignore-duplicate nil))
  (use-package company-lsp :commands company-lsp)
  (use-package helm-lsp :commands helm-lsp-workspace-symbol)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
  ;; optionally if you want to use debugger
  (use-package dap-mode)

  (use-package company
    :diminish (company-mode . "➨")
    :defines
    company-dabbrev-downcase
    :hook
    (prog-mode . company-mode)
    (after-init . global-company-mode)
    :config
    (use-package company-irony)
    (use-package company-anaconda)
    (setq-default company-idle-delay              0
                  company-minimum-prefix-length   2
                  company-show-numbers            t
                  company-tooltip-limit           20
                  company-dabbrev-downcase        nil
                  company-tooltip-flip-when-above t
                  company-backends                '(company-anaconda
                                                    company-bbdb
                                                    company-clang
                                                    company-dabbrev-code
                                                    company-elisp
                                                    company-files
                                                    company-gtags
                                                    company-lsp
                                                    company-omnisharp
                                                    company-yasnippet)))

  (use-package omnisharp
    :diminish (omnisharp-mode . "⃝")
    :if not-win
    :hook
    (csharp-mode . omnisharp-mode)
    :config
    ()))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  :hook
  (prog-mode . smartparens-strict-mode))

(use-package hyperbole
  :demand t
  :diminish hyperbole-mode
  :config
  (require 'hyperbole))

(use-package evil-org
  :diminish evil-org-mode
  :defines evil-normal-state
  :init
  (require 'evil-org)
  (require 'evil-org-agenda)
  :hook
  (org-mode . evil-org-mode))

(use-package evil-magit
  :after magit
  :hook
  (magit-mode . evil-magit-init))

(use-package org-ref
  :if not-win
  :init
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
        org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
        org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"))

(use-package org
  :defines
  kc/agenda-dir
  kc/diary-file
  kc/notes-file
  kc/refile-file
  kc/org-all-agenda-files
  org-agenda-span
  org-agenda-clockreport-parameter-plist
  org-agenda-window-setup
  org-agenda-skip-scheduled-if-done
  org-agenda-skip-deadline-if-done
  org-agenda-skip-timestamp-if-done
  org-agenda-log-mode-items
  org-capture-templates
  :functions
  evil-org-agenda-set-keys
  evil-org-set-key-theme
  :config
  (setq-default org-directory (if not-win
                                  "~/Dropbox/org"
                                "~/../../OneDrive/org")
                kc/agenda-dir (concat org-directory "")
                kc/org-all-agenda-files (directory-files
                                         (expand-file-name kc/agenda-dir) t org-agenda-file-regexp)
                org-agenda-span 'day
                org-fontify-quote-and-verse-blocks t
                org-agenda-file-regexp "\\`[^.].*\\.org\\'"
                org-use-fast-todo-selection t
                org-hide-emphasis-markers t
                org-treat-S-cursor-todo-selection-as-state-change nil
                org-ellipsis "⤵"
                org-clock-continuously t
                org-clock-out-remove-zero-time-clocks t
                org-log-done 'time
                org-refile-targets (quote ((nil :maxlevel . 1) (kc/org-all-agenda-files :maxlevel . 2)))
                org-catch-invisible-edits 'smart
                org-agenda-clockreport-parameter-plist
                '(:link t :maxlevel 4 :fileskip0 t :formula %
                        :properties ("Request" "Phase" "TaskNbr"))
                org-deadline-warning-days 45
                org-agenda-window-setup 'current-window
                org-agenda-skip-scheduled-if-done t
                org-agenda-skip-deadline-if-done t
                org-agenda-skip-timestamp-if-done t
                org-agenda-log-mode-items '(closed clock state)
                org-columns-default-format
                "%25ITEM(Task) %40Description %20Captured %10Effort(Effort){:} %10CLOCKSUM"
                org-global-properties
                (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                        ("STYLE_ALL" . "habit")))
                org-todo-keywords
                (quote ((sequence "TODO(t)" "WIP(n)" "|" "DONE(d)" "CANCELLED(c/!)")
                        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                                  "CANCELLED(c/!)" "PHONE" "MEETING")))
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
                   "* TODO %?       :%^G:
  :PROPERTIES:
  :Captured: %U
  :Client: %^{Client}
  :END:\n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
                   :clock-in t :clock-resume t :prepend t)
                  ("p" "Phone call" entry
                   (file kc/refile-file)
                   "* PHONE %?
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %K
  :END:" :clock-in t :clock-resume t)
                  ("i" "Interuption" entry
                   (file kc/refile-file)
                   "* %?
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %K
  :END:
#+begin_quote\n%x\n#+end_quote\n" :clock-in t :clock-resume t)
                  ("s" "Source Note" entry
                   (file kc/refile-file)
                   "* %?
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %K
  :END:
#+begin_source %^{Language|conf|csharp|powershell|sgml|shell|sql}\n%x\n#+end_source\n" :clock-in t :clock-resume t)
                  ("j" "Journal" entry
                   (file+olp+datetree kc/diary-file)
                   "* %?
  :PROPERTIES:
  :Captured: %U
  :Category: %^{Entry type|Bible|Note|Journal}
  :END:" :clock-in t :clock-resume t)
                  ("n" "Note" entry
                   (file kc/notes-file)
                   "* %? :NOTE:
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %K
  :END:" :clock-in t :clock-resume t)
                  ("m" "Meeting" entry
                   (file kc/notes-file)
                   "* MEETING %?
  :PROPERTIES:
  :Captured: %U
  :Prev_Loc: %K
  :END:" :clock-in t :clock-resume t)
                  ("b" "Bookmark" entry
                   (file+headline kc/notes-file "Bookmarks")
                   "* %?\n:PROPERTIES:\n:Captured: %U\n:END:\n\n" :empty-lines 1))
                org-clock-in-switch-to-state
                (defun kc/clock-in-to-wip (kw)
                  "Switch from TODO to WIP when clocking in."
                  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
                    (cond
                     ((member (org-get-todo-state) (list "TODO"))
                      "WIP")
                     (t
                      kw)))))
  :general
  (kc/mode-leader-keys
    :keymaps 'org-mode-map
    "P" 'org-set-property
    "tic" 'org-table-insert-column
    "tir" 'org-table-insert-row
    "ti-" 'org-table-insert-hline
    "tdc" 'org-table-delete-column)
  ;; (kc/leader-keys
  ;;   :keymaps 'org-agenda-mode-map
  ;;   :states '(normal motion)
  ;;   "TAB" '(switch-to-prev-buffer)
  ;;   "ff"  'helm-find-files
  ;;   "fl"  'helm-locate
  ;;   "SPC" 'helm-M-x
  ;;   "bd"  'kill-this-buffer
  ;;   "bb"  'helm-buffers-list)
  (kc/mode-leader-keys
    :keymaps 'org-agenda-mode-map
    "l" 'org-agenda-log-mode
    "r" 'org-agenda-clockreport-mode
    "TAB" '(switch-to-prev-buffer :which-key "find files")
    "ff"  'helm-find-files
    "fl"  'helm-locate
    "SPC" 'helm-M-x
    "bd"  'kill-this-buffer
    "bb"  'helm-buffers-list)
  :hook
  (org-mode . flyspell-mode)
  :config
  (evil-normal-state)
  (evil-org-set-key-theme '(insert textobjects additional calendar))
  (evil-org-agenda-set-keys)
  (add-hook 'org-agenda-finalize-hook #'(lambda () (evil-normal-state))))

  ;; (use-package org-msg
  ;;   :defer nil
  ;;   :config
  ;;   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil"
  ;;         org-msg-startup "hidestars indent inlineimages"
  ;;         org-msg-greeting-fmt "\nHi %s,\n\n"
  ;;         org-msg-greeting-fmt-mailto t
  ;;         org-msg-signature "

  ;; Thanks,

  ;; ,#+begin_signature
  ;; -- *K. C.* \\\\
  ;; =juntunen.kc@gmail.com=
  ;; ,#+end_signature")
  ;;   (org-msg-mode))

(if not-win
    (load-file "~/.emacs.d/lin-init.el")
  (load-file "~/.emacs.d/win-init.el"))

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
                          ,load-file-name elapsed))
               (persistent-scratch-restore))))
(provide 'init)
;;; init.el ends here
