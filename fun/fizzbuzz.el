;;;
;;; Fizz Buzz
;;;

(require 'cl)

(defun fizzbuzz-loop (a b)
  "Use loop from CL to list fizzbuzz"
  (loop for i from a to b
	if (zerop (mod i 3)) collect "fizz" and collect i into fizz
	if (zerop (mod i 5)) collect "buzz" and collect i into buzz
	unless (memq i (append fizz buzz)) collect (number-to-string i)
	collect "\n"))

(defun fizzbuzz-dotimes (a b)
  "Use dotimes to list fizzbuzz"
  (let (fizzbuzz)
    (dotimes (i (- b a))
      (or (let ((f (when (zerop (mod (1+ i) 3)) (push "fizz" fizzbuzz)))
		(b (when (zerop (mod (1+ i) 5)) (push "buzz" fizzbuzz))))
	    (or f b))
	  (push (number-to-string (1+ i)) fizzbuzz))
      (push "\n" fizzbuzz))
    (nreverse fizzbuzz)))

(defun fizzbuzz (&optional a b)
  "return the fizz buzz string counting from a to b"
  (interactive)
  (let ((a (or a 1))
	(b (or b 24)))
    (with-current-buffer (get-buffer-create "*fizzbuzz*")
      (erase-buffer)
      (mapc 'insert (fizzbuzz-loop a b)))
      ;; (mapc 'insert (fizzbuzz-dotimes a b)))
    (set-window-buffer (selected-window) "*fizzbuzz*")))
