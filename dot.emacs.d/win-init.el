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
(kc/leader-keys
  "cqn" 'kc/copy-query-notes
  "cqi" 'kc/copy-query-incidents)

(server-start)
