(defparameter *location* 'living-room)

(defparameter *nodes* '((living-room (you are in the living-room. a "WiZard" is snoring loudly on the couch.))
						(garden (you are in a beautiful garden. there is a well in front of you.))
						(attic (you are in the attic. there is a "giant," rusty welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door) (attic upstairs ladder))
						(garden (living-room east door))
						(attic (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
								   (bucket living-room)
								   (chain garden)
								   (frog garden)))

(defun describe-location (location nodes)
	(cadr (assoc location nodes)))

(defun describe-paths (location edges)
	(labels ((describe-path (edge)
		`(there is a ,(caddr edge) going ,(cadr edge) from here.)))
	(apply #'append (mapcar #'describe-path (cdr (assoc location edges))))))

(defun objects-at (location objects obj-locations)
	(labels ((at-loc-p (obj) 
		(eq (cadr (assoc obj obj-locations)) location)))
	(remove-if-not #'at-loc-p objects)))

(defun describe-objects (location objects obj-locations)
	(labels ((describe-obj (obj)
		`(you see a ,obj on the floor.)))
	(apply #'append (mapcar #'describe-obj (objects-at location objects obj-locations)))))

(defun look ()
	(append (describe-location *location* *nodes*)
			(describe-objects *location* *objects* *object-locations*)
			(describe-paths *location* *edges*)))

(defun walk (direction)
	(let ((next (find direction (cdr (assoc *location* *edges*)) :key #'cadr)))
		(if next
			(progn (setf *location* (car next)) 
				   (look))
			'(you cannot go that way.))))

(defun pickup (object)
	(cond ((member object (objects-at *location* *objects* *object-locations*))
		       (push (list object 'body) *object-locations*)
		       `(you are now carrying the ,object))
		   (t '(you cannot get that.))))

(defun inventory ()
	(cons 'items- (objects-at 'body *objects* *object-locations*)))


;; custom game REPL

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-repl () 
	(let ((cmd (game-read)))
		(unless (eq (car cmd) 'quit)
			(game-print (game-eval cmd))
			(game-repl))))

(defun game-read ()
	(let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
		(flet ((quote-it (x) (list 'quote x)))
			(cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (cmd)
	(if (member (car cmd) *allowed-commands*)
		(eval cmd)
		'(i don't know that command.)))

(defun tweak-text (text caps literal) 
	(when text
		(let ((item (car text))
			  (rest (cdr text)))
		(cond ((eq item #\space) (cons item (tweak-text rest caps literal)))
			  ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t literal)))
			  ((eq item #\") (tweak-text rest caps (not literal)))
			  (literal (cons item (tweak-text rest nil literal)))
			  (caps (cons (char-upcase item) (tweak-text rest nil literal)))
			  (t (cons (char-downcase item) (tweak-text rest caps literal)))))))

(defun game-print (text)
	(princ 
		(coerce 
			(tweak-text 
				(coerce (string-trim "() " (prin1-to-string text)) 'list) 
				t nil) 
			'string))
	(fresh-line))











