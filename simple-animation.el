(defun do-animation-progress (times go-to-char)
  (let ((chars '(?- ?\\ ?| ?/ ?- ?\\ ?| ?/))
	(buff (get-buffer-create "*Simple Animation*")))
    
    (display-buffer buff)

    ;; Execute the animation...
    (with-current-buffer buff nil nil
			 ;; Make sure the buffer is clean
			 (erase-buffer)
			 ;; Add characters to facilitate management
			 (self-insert-command 5 ?\ )
			 ;; Set the point to the new location
			 (goto-char go-to-char)
			 ;; Make the list circular
			 (setcdr (last chars) chars)
			 ;; Run the animation...
			 (animation-progress chars times)
			 ;; Show the animation is finished
			 (animation-update ?🗸))))

(defun animation-progress (l n)
  (when (> n 0)
    ;; Update buffer
    (animation-update (car l))
    ;; Sleep and redisplay the screen
    (sleep-for 0.5)
    (sit-for 0)
    ;; Continue with the animation
    (animation-progress (cdr l) (- n 1))))

(defun animation-update (char)
  ;; Delete previews animation state
  (delete-backward-char 1)
  ;; Insert new animation state
  (self-insert-command 1 char))

;;
;; Call the actual animation function
;;
(make-thread (lambda () (do-animation-progress 20 3)) "thread-simple-animation")
