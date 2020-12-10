(ql:quickload 'cl-ppcre)

(setf double-newline (coerce '(#\Newline #\Newline) 'string))

(defun read-file (infile)
  "Reads a file into a single string."
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun records<-file (fn)
  (cl-ppcre:split double-newline (read-file fn)))

(defun sum-counts (fn)
  (apply #'+ (mapcar #'count-uniques-for-group (records<-file fn))))

(defun count-uniques-for-group (record)
  (let ((ht (make-hash-table)))
    (loop :for char :across (remove #\Newline (remove #\Space record))
	  :do (setf (gethash char ht) t)
	  :finally (return (hash-table-count ht)))))
