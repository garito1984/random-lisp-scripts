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

(defun parse-command-line-arguments ()
  "Return a PLIST with valid script options"
  (let ((cmd-opts (mapcar (lambda (opt)
			    (cond ((equal opt "--add-days") :add-days)
				  ((equal opt "--base-date") :base-date)
				  (t opt)))
			  command-line-args-left))
	(opts nil)
	l r) ; left: flag, right: value
    ;; Process command line arguments...
    (while (prog1
	       (setq l (pop cmd-opts))
	     (setq r (pop cmd-opts)))
      (cond ((eq l :add-days)
	     (push (if r (string-to-number r)) opts)
	     (push l opts))
	    ((eq l :base-date)
	     (push (if r (date-to-time r)) opts)
	     (push l opts))
	    (t (push r cmd-opts)))) ; Since l wasn't a flag push r back
    opts))

(defun get-script-configuration ()
  (let* ((opts (parse-command-line-arguments))
	 (base (plist-get opts :base-date)))
    (plist-put opts :base-date (or base (current-time)))))

;;;
;;; Script starts here
;;;

(let* ((options (get-script-configuration))
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
