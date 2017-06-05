(defparameter *big* 100)
(defparameter *small* 1)
(defparameter *guess* 50)

(defun guess-my-number ()
	(cond 
		((<= (- *big* *small*) 0) 
			(setf *guess* *small*)
			(correct))
		(t 
			(format t "Guessing a number between ~a and ~a ~%" *small* *big*)
			(setf *guess* (+ *small* (random (- *big* *small*))))))
	(print *guess*))

(defun bigger ()
	(setf *small* (+ *guess* 1))
	(guess-my-number))

(defun smaller ()
	(setf *big* (- *guess* 1))
	(guess-my-number))

(defun correct ()
	(format t "I knew it was ~a ~%" *guess*)
	(start-over))

(defun start-over ()
	(setf *small* 1)
	(setf *big* 100)
	(guess-my-number))