(defun lower (range)
  (list (car range) (floor (/ (apply #'+ range) 2))))

(defun higher (range)
  (list (floor (/ (apply #'+ 1 range) 2)) (cadr range)))

(defun get-row (specs)
  (let ((range '(0 127)))
    (loop :for step :from 0 :to 6
	  :do (setf range (if (eq #\F (nth step specs))
			      (lower range)
			      (higher range)))
	  :finally (return (car range)))))

(defun get-seat (specs)
  (let ((range '(0 7)))
    (loop :for step :from 7 :to 9
	  :do (setf range (if (eq #\L (nth step specs))
			      (lower range)
			      (higher range)))
	  :finally (return (car range)))))

(defun get-seat-id (spec-string)
  (let ((specs (coerce spec-string 'list)))
    (+ (get-seat specs) (* 8 (get-row specs)))))
    
(defun strings<-file (fn)
  (with-open-file (stream fn)
    (loop :for line = (read-line stream nil)
          :while line
          :collect line)))

(reduce #'max (mapcar #'get-seat-id (strings<-file "input.txt")))

(defun find-missings (fn)
  (let ((roster (mapcar #'get-seat-id (strings<-file "input.txt"))))
    (loop :for i :from 0 :to (reduce #'max roster)
	  :with missing = ()
	  :do (if (not (member i roster))
		  (setf missing (cons i missing)))
	  :finally (return missing))))
	  
	  
