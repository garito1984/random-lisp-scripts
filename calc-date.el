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

(defun calc-date-message (past-date days)
  (let ((future-date (time-add past-date (days-in-seconds days))))
    (concat (format "Adding %d days to " days)
	    (format-time-string "%+4Y-%m-%d becomes " past-date)
	    (format-time-string "%A, %B %-e, %Y.\n" future-date))))

(defun parse-script-options ()
  "Return a PLIST with valid script options"

  ;;
  ;; Turn the command line args into a plist...
  ;;
  (let ((opts '(:base-date nil :add-days nil)))

    ;; The logic assumes a value follows each flag, example: --da-flag da-value
    (let ((flag nil))
      (mapcar (lambda (opt)
		(cond ((equal opt "--add-days")  (setq flag :add-days))
		      ((equal opt "--base-date") (setq flag :base-date))
		      (t (plist-put opts flag opt))))
	      command-line-args-left))
    
    ;; Try to parse the argument for :add-days
    (let ((add-days (plist-get opts :add-days)))
      (plist-put opts
		 :add-days
		 (if add-days (string-to-number add-days) nil)))

    ;; Try to parse the argument for :base-date
    (let ((base-date (plist-get opts :base-date)))
      (plist-put opts
		 :base-date
		 (if base-date (date-to-time base-date) (current-time))))

    opts))

;;;
;;; Script starts here
;;;

(let* ((options (parse-script-options))
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
