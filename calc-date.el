#!/usr/bin/emacs -x

;;;
;;; Function declarations
;;;

(defun days-in-seconds (days)
  "Return a DAYS number of seconds"
  (let ((seconds 60)  ;; 1 minute
	(minutes 60)  ;; 1 hour
	(hours   24)) ;; 1 day
    (* seconds minutes hours days)))

(defun calc-date-message (days)
  (let ((future-date (time-add (current-time) (days-in-seconds days))))
    (concat (format "In %d days will be " days)
	    (format-time-string "%A, %B %-e, %Y.\n" future-date))))

(defun parse-script-options ()
  "Return a PLIST with valid script options or nil if no valid options are found"
  (and command-line-args-left
       (equal (length command-line-args-left) 2)
       (equal (pop command-line-args-left) "--add-days")
       (string-match-p "^[[:digit:]]+$" (car command-line-args-left))
       (list :add-days (string-to-number (pop command-line-args-left)))))

;;;
;;; Script starts here
;;;

(let ((days (plist-get (parse-script-options) :add-days)))
  (when (not days)
    ;; Exit with an error and notify the user about proper usage 
    (send-string-to-terminal "Usage: calc-date.el --add-days DAYS\n")
    (kill-emacs 1))

  ;; The options are correct, start with the processing...
  (send-string-to-terminal (calc-date-message days))
  (kill-emacs 0))
