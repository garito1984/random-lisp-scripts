#!/usr/bin/emacs -x

(load (concat (file-name-directory load-file-name) "calc-date-lib.el") nil t t)

(defun send-string (string)
  (send-string-to-terminal string))

(let* ((options (parse-command-line-arguments))
       (days (plist-get options :add-days))
       (base-date (plist-get options :base-date)))
  (when (not days)
    ;; Exit with an error and notify the user about proper usage 
    (send-string "Usage: calc-date.el --add-days DAYS\n")
    (send-string "       calc-date.el --base-date 2024-12-25 --add-days DAYS\n")
    (kill-emacs 1))

  ;; The options are correct, start with the processing...
  (send-string (calc-date-message base-date days))
  (kill-emacs 0))
