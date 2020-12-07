(setf testline "1-3 a: abcde")

(setf *passwords* '("1-3 a: abcde"
		   "1-3 b: cdefg"
		    "2-9 c: ccccccccc"))

(defun specs<-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-password-spec (spec)
  (destructuring-bind
      (minmax letter password)
      (cl-utilities:split-sequence #\Space spec)
    (destructuring-bind
	(min max)
	(cl-utilities:split-sequence #\- minmax)
      ;; output
      (list (parse-integer min)
	    (parse-integer max)
	    (coerce (remove #\: letter) 'character)
	    password))))

(defun correct-freq-p (spec)
  (destructuring-bind
      (min max chr password)
      spec
    (loop :for char :across password
	  :with count = 0
	  :until (> count max)
	  :do (if (eq chr char)
		  (incf count))
	  :finally (return (<= min count max)))))

(defun check-all-passwords (spec-list)
  (mapcar (lambda (spec)
	    (correct-freq-p (parse-password-spec spec)))
	  spec-list))

(defun count-correct-passwords (filename)
  (count t (check-all-passwords (specs<-file filename))))


  
