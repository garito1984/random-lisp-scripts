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
  (let ((cmd-opts (copy-tree command-line-args-left))
	(opts nil)
	l r) ; left right
    ;; Process command line arguments...
    (while (progn (setq l (pop cmd-opts))
		  (setq r (pop cmd-opts))
		  l)
      (cond ((equal l "--add-days")
	     (push :add-days opts)
	     (push (string-to-number r) opts))
	    ((equal l "--base-date")
	     (push :base-date opts)
	     (push (if r (date-to-time r)) opts))
	    (t (push r cmd-opts)))) ; Return r to check if it is a flag
    (setq opts (reverse opts))
    
    ;; Set required args as needed...
    (let ((base (plist-get opts :base-date)))
      (plist-put opts :base-date (or base (current-time))))
    ))

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
