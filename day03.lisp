(defun tree-rows<-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun count-trees (tree-rows right down)
  (let ((col-count (length (car tree-rows)))
	(move-right right)
	(move-down  down))
    
    (loop :for row :in tree-rows
	  :with count = 0
	  :with colindex = 0
	  :with rowindex = 0
	  :do (if (zerop (mod rowindex move-down))
		  (incf count (tree-1-or-0 row colindex)))
	      (incf rowindex move-down)
	      (setf colindex (mod (incf colindex move-right) col-count))
	  :finally (return count))))
		   
(defun tree-1-or-0 (str pos)
  (if (eq #\# (char str pos)) 1 0)) 
		    
(setf pairs '((1 1) (3 1) (5 1) (7 1) (1 2)))

(defun count-trees-at-different-slopes (filename)
  (let ((rows (tree-rows<-file filename)))
    (mapcar (lambda (pair) (destructuring-bind
			       (rt dn)
			       pair
			   (count-trees rows rt dn)))
	    pairs)))

(apply #'* (count-trees-at-different-slopes "day03testinput.txt"))


