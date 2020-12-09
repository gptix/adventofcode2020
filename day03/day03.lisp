(defun strings<-file (fn)
  (with-open-file (stream fn)
    (loop :for line = (read-line stream nil)
          :while line
          :collect line)))

;; Convert each row of characters to a list of ints (0 or 1)
;; to make math simpler later.

(defun zeroes-and-ones<-string (str)
  (mapcar (lambda (c) (if (eq c #\#) 1 0)) (coerce str 'list)))

(defun z1-lists<-strings (list-of-strings)
  (mapcar #'zeroes-and-ones<-string list-of-strings))

(defun list-of-z1-lists<-file (fn)
  (z1-lists<-strings (strings<-file fn)))

(defun encountered-trees (rows dx dy)
  (let ((row-count (length rows))
	(col-count (length (car rows))))
	(loop :with total = 0
	      :with y = 0
	      :with x = 0
	      :while (< y row-count)
	      :do (incf total (nth x (nth y rows)))
;;		  (format t "[row:~a][col:~a] = ~a~%" y x (nth x (nth y rows)))
		  (incf y dy)
		  (setf x (mod (+ x dx) col-count))
	      :finally (return total))))

(setf coordinate-pairs '((1 1) (3 1) (5 1) (7 1) (1 2)))


(defun get-counts (filename coord-pairs)
  (let ((rows (list-of-z1-lists<-file filename)))
    (mapcar (lambda (pair)
	      (destructuring-bind
		  (dx dy)
		  pair
		(encountered-trees rows dx dy)))
	    coord-pairs)))






;; deprecated

(defun wait-for-enter ()
  (format *query-io* "<ENTER> to proceed -")
  (read-line *query-io*))

(defun tree-rows<-file (filename)
  (with-open-file (stream filename)
    (loop :for line = (read-line stream nil)
          :while line
          :collect line)))

		   
(defun tree-1-or-0 (str pos)
  (if (eq #\# (char str pos)) 1 0)) 
		    
(defun count-trees-at-different-slopes (filename)
  (let ((rows (tree-rows<-file filename)))
    (mapcar (lambda (pair) (destructuring-bind
			       (rt dn)
			       pair
			   (count-trees rows rt dn)))
	    pairs)))

(apply #'* (count-trees-at-different-slopes "day03testinput.txt"))




		     
(defun count-trees (rows dx dy)
  (let ((row-count (length rows))
	(col-count (length (car rows)))
;;    (print (length rows))))
    (loop :with tree-count = 0
	  :with col-index = 0
	  :with row-index = 0
	  :while (< row-index row-count) ;; remember zero index vs length
	  :do (format t "row-index: ~a -- col-index: ~a -- tree?: ~a~%" rowindex col-index (tree-1-or-0 (nth row-index rows)))
	      (incf tree-count (tree-1-or-0 (nth row-index rows) col-index)))
	      (incf row-index dy)
	      (setf col-index (mod (incf col-index dx) col-count))
	  :finally (return tree-count))))


