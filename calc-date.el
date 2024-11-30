#!/usr/bin/emacs -x

;;;
;;; Declarations
;;;

(setq script-flags '("--add-days"  :add-days
		     "--base-date" :base-date))

(setq handle-flag-values
      '(:add-days  (lambda (val) (if val (string-to-number val)))
	:base-date (lambda (val) (if val (date-to-time val)))))

(defun parse-command-line-arguments ()
  "Return a PLIST from the script's command line arguments"
  (let ((cmd-opts (mapcar (lambda (opt)
			    (or (plist-get script-flags opt 'equal) opt))
			  command-line-args-left))
	(opts nil))
    ;; Process command line arguments...
    (while (consp cmd-opts)
      (let ((l (pop cmd-opts))
	    (r (pop cmd-opts)))
	(cond ((and (symbolp l) (symbolp r))) ; Consecutive flags, skipping...
	      ((symbolp l)
	       (push (funcall (plist-get handle-flag-values l) r) opts)
	       (push l opts))
	      (t (push r cmd-opts))))) ; Since l wasn't a flag push r back
    opts))

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

;;;
;;; Script starts here
;;;

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
