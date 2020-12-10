(setf file-string (read-file "test-input.txt"))
(setf recs (records<-string file-string))
(setf 1-rec (car recs))
(setf 1-rec-kvp-strings (kvps<-record 1-rec))
(setf 1-rec-pairs (pairs<-kvpstrings 1-rec-kvp-strings)))
(setf one-pair (car 1-rec-pairs))
(setf test-ht (make-hash-table :test 'equal))
(ht-entry<-pair test-ht one-pair)


;; read file into string
;; split string into records
;; set valid-count to 0
;; create hash-table

;; for each record
;; create 
;;   split record into pairs

;;   for each record
;;     blow into hash-table
;;     confirm that there are required fields
;;     confirm that each field's value is valid

;;     if so, increment count
;;     empty hash-table
