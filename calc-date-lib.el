;;;
;;; Declarations
;;;
(setq script-flags '("--add-days"  :add-days
		     "--base-date" :base-date))

(setq handle-flag-values
      '(:add-days  (lambda (val) (if val (string-to-number val)))
	:base-date (lambda (val) (if val (date-to-time val)))))

(defun parse-command-line-arguments (&optional params)
  "Return a PLIST from the script's command line arguments"
  (let ((cmd-opts (mapcar (lambda (opt)
			    (or (plist-get script-flags opt 'equal) opt))
			  (or params command-line-args-left)))
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
