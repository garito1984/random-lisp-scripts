



(defun do-animation-progress (times go-to-char)
  (goto-char go-to-char)
  (let ((chars '(?- ?\\ ?| ?/ ?- ?\\ ?| ?/)))
    (setcdr (last chars) chars) ; Make the list circular
    (animation-progress chars times)
    (animation-update ?🗸)))

(defun animation-progress (l n)
  (when (> n 0)
    (animation-update (car l))
    (sleep-for 1)
    (sit-for 0) ; Need this to see the change on screen
    (animation-progress (cdr l) (- n 1))))

(defun animation-update (char)
  (delete-backward-char 1)
  (self-insert-command 1 char))

;; Call the actual animation function
(do-animation-progress 20 3)

