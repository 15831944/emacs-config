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

;; (defvar kc/fixed-width-font "Anka/Coder Condensed-10:style=Regular" "Default monospace font.")
(defvar kc/fixed-width-font "Victor Mono SemiBold" "Default monospace font.")

(set-face-attribute 'default nil :family kc/fixed-width-font :height 120 :weight 'normal)

(provide 'early-init)
;;; init.el ends here
