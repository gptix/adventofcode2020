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


  
;; part two

(defun parse-password-spec-2 (spec)
  (destructuring-bind
      (positions letter password)
      (cl-utilities:split-sequence #\Space spec)
    (destructuring-bind
	(pos1 pos2)
	(cl-utilities:split-sequence #\- positions)
      ;; output
      (list (parse-integer pos1)
	    (parse-integer pos2)
	    (coerce (remove #\: letter) 'character)
	    password))))

(defun correct-freq-p-2 (spec)
  (destructuring-bind
      (pos1 pos2 chr password)
      spec
    ;; indexing is 1 based
    ;; password is OK IFF there is one match
    (= 1 (+ (if (eq chr (char password (- pos1 1))) 1 0)
	    (if (eq chr (char password (- pos2 1))) 1 0)))))

(defun check-all-passwords-2 (spec-list)
  (mapcar (lambda (spec)
	    (correct-freq-p-2 (parse-password-spec-2 spec)))
	  spec-list))

(defun count-correct-passwords-2 (filename)
  (count t (check-all-passwords-2 (specs<-file filename))))
