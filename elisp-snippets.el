;;
;; This file uses buffer-variables which means the code will work
;; only when called on the same buffer from which it was evaluated
;;
(let (seq seq-reset)
  (defun get-fibonacci (&optional reset whole-list)
    (let (res)
      ;; Init/Reset
      (funcall seq-reset reset)
      ;; Calculate fibonacci
      (setq seq (cons (+ (car seq) (cadr seq)) seq))
      (if whole-list seq
	(car seq))))

  (setq seq-reset (lambda (reset)
		    (cond ((or reset (not seq))
			   (setq seq (list 2 1)))))))

(let ((wl (window-list))
      (w nil))
  ;; Find the NOT current window
  (while (equal (setq w (pop wl)) (selected-window)))
  (if w
      (let ((b (window-buffer w)))
	;; If other window exists, show it
	(list (buffer-name b) w (buffer-file-name b)))))

(let ((el nil)
      (res nil))
 (dolist (el '("fer" "sol" "mara") res)
	(insert (format "%s\n" (setq res (upcase el))))))
