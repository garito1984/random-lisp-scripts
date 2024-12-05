#!/usr/bin/emacs -x

(load (concat (file-name-directory load-file-name) "calc-date-lib.el") nil t t)

(let* ((options (parse-command-line-arguments))
       (days (plist-get options :add-days))
       (base-date (plist-get options :base-date)))
  (when (not days)
    ;; Exit with an error and notify the user about proper usage 
    (send-string-to-terminal "Usage: calc-date.el --add-days DAYS\n")
    (send-string-to-terminal "       calc-date.el --base-date 2024-12-25 --add-days DAYS\n")
    (kill-emacs 1))

  ;; The options are correct, start with the processing...
  (send-string-to-terminal (calc-date-message base-date days))
  (kill-emacs 0))


