;; win-init.el --- Windows-specific configuration. -*- lexical-binding: t -*-
;; Code:
(message "Using Windows. :-(")
(setq inhibit-compacting-font-caches t
      w32-pipe-read-delay 0)

(setq ispell-program-name "C:/Users/k.c.juntunen/opt/Hunspell/bin/hunspell.exe"
      ispell-local-dictionary "en_US"
      ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

(defun kc/copy-query-notes ()
  "Copy a query string to the clipboard for the `notes'
table for the last hour."
  (interactive)
  (w32-set-clipboard-data
   (format "SELECT * FROM notes WHERE NoteDateTime > '%s' AND Tag1 LIKE '%%ATHOS%%'"
           (format-time-string "%Y-%m-%d %H:%M:%S"
                               (time-subtract (current-time)
                                              (seconds-to-time (* 60 60 30)))
                               "wall"))))

(defun kc/copy-query-incidents ()
  "Copy a query string to the clipboard for the `incidents'
table for the last hour."
  (interactive)
  (w32-set-clipboard-data
   (format "SELECT * FROM incidents WHERE DateAdded > '%s' AND Resolved <> 'Y'"
           (format-time-string "%Y-%m-%d %H:%M:%S"
                               (time-subtract (current-time)
                                              (seconds-to-time (* 60 60 30)))
                               "wall"))))
(defvar request-looker-upper-path
  "C:/Users/k.c.juntunen/opt/GetProject/Experimental.exe"
  "The program that pulls in Projects/Phases/Tasks in Org format.")

(defun kc/get-request-data (request-number)
  "Insert Project/Phase/Task data into buffer in Org format."
  (interactive "sRequestNbr: ")
  (shell-command
   (format "%s %s"
           request-looker-upper-path request-number)))

(kc/leader-keys
  "yqn" 'kc/copy-query-notes
  "yqi" 'kc/copy-query-incidents
  "yrg" 'kc/get-request-data)

(server-start)
(provide 'win-init)
