(ql:quickload 'cl-ppcre)

;; Handy
(setf double-newline (coerce '(#\Newline #\Newline) 'string))
(setf required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
(setf optional-field "cid")



;; Test data located below!


;;; Munge input file into data structure for main algo.

(defun read-file (infile)
  "Reads a file into a single string."
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun records<-string (str)
  "Splits a file (single string) on double newlines, into records."
  (cl-ppcre:split double-newline str))

(defun kvps<-record (rec)
  "Splits a record on whitespace, into key:value strings."
  (cl-ppcre:split "\\s+" rec))

(defun list-of-kvps<-records (lor)
  "Wrapper"
  (mapcar #'kvps<-record lor))

(defun pair<-kvpstring (kvpstring)
  (cl-ppcre:split #\: kvpstring))

(defun pairs<-kvpstrings (kvps)
  "Wrapper."
  (mapcar #'pair<-kvpstring kvps))

(defun ht-entry<-pair (ht pair)
  (destructuring-bind
      (k v)
      pair
    (setf (gethash k ht) v)))

(defun ht-entries<-pairs (ht pairs)
  "Wrapper"
  (mapcar (lambda (pair) (ht-entry<-pair ht pair)) pairs))





;;; Used for part 1, which did not validate contents of fields
  
(defun k<-kvp (kvp)
  "Splits the key off of a kv pair. For part one, we do not need the value."
  (car (cl-ppcre:split ":" kvp)))

(defun ks<-kvps (kvps)
  "Wrapper."
  (mapcar #'k<-kvp kvps))

;; Function that munges a file into data for use by algorithm
(defun list-of-ks<-file (filename)
  "Input: file
   Output: list of lists of keys"
  (mapcar #'ks<-kvps (list-of-kvps<-records
		      (records<-string
		       (read-file filename)))))








;;; Validate records

(defun string-in-list-p (str lst)
  "Helper function. Returns t if str is in list, otherwise nil."
  (if (member-if (lambda (el) (string= el str)) lst) t))

(defun all-required-fields-p (ks)
  "Checks to see if all required keys are present."
  (loop :for rf :in required-fields
	:with valid = t
	:while valid
	:do (setf valid (string-in-list-p rf ks))
	:finally (return valid)))


;;; Apply algorithm to file.

(defun valid-record-count<-filename (fn)
  (count t (mapcar #'all-required-fields-p (list-of-ks<-file fn))))



;;; Field validation

(setf decimal-numerals '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun decimal-numeral-p (chr)
  (if (member chr decimal-numerals) t nil))

(defun all-decimals-p (str)
  (every #'identity (mapcar #'decimal-numeral-p (coerce str 'list))))

(setf hex-numerals '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))

(defun hex-numeral-p (chr)
  (if (member chr hex-numerals) t nil))

(defun all-hex-p (str)
  (every #'identity (mapcar #'hex-numeral-p (coerce str 'list))))




(defun in-range (number range)
  (<= (first range) number (second range)))

(defun string-in-range (str range)
  (<= (first range) (parse-integer str) (second range)))





(defun valid-year-string-p (str range)
  (and (all-decimals-p str)
       (string-in-range (parse-integer str) range)))

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
(setf byr-range '(1920 2002))
(defun valid-byr-p (str)
  (valid-year-string-p str byr-range))
     
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(setf iyr-range '(2010 2020))
(defun valid-iyr-p (str)
  (valid-year-string-p str iyr-range))

;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(setf eyr-range '(2020 2030))
(defun valid-eyr-p (str)
  (valid-year-string-p str eyr-range))



;; hgt (Height) - a number followed by either cm or in:
;; If cm, the number must be at least 150 and at most 193.
;; If in, the number must be at least 59 and at most 76.
(setf hgt-cm-range '(150 193))   
(setf hgt-in-range '(59 76))

(defun valid-hgt-p (str)
  (let ((strlen (length str)))
    
    (and (<= 4 strlen 5) ;; validate length of datum
	 (let ((unit (subseq str (- strlen 2))) ; get possible unit
	       (number (subseq str 0 (- strlen 2)))) ; get possible number

	   (and (string-in-list-p unit '("in" "cm")) ; validate unit
		(all-decimals number) ; validate that this is a decimal number
		(let ((range (if (string= unit "cm") hgt-cm-range hgt-in-range))) ; select range to use

		  (in-range (parse-integer number) range)))))) ; validate that number is in range


;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f. A hex number.
(defun valid-hcl-p (str)
  (and (= 6 (length str))
       (all-hex-p str)))

  
;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(setf ecl-values '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
(defun valid-ecl-p (str)
  (if (member-if (lambda (s) (string= str s)) ecl-values) t nil))

  
;; pid (Passport ID) - a nine-digit number, including leading zeroes.
(defun valid-pid-p (str)
  (if (and (= 9 (length str))
	   (all-decimals-p str)) t))

;; cid (Country ID) - ignored, missing or not.




;;; Test data

(setf good-fields '("iyr" "byr" "eyr" "hgt" "hcl" "ecl" "pid"))
(setf bad-fields '("byr" "eyr" "hgt" "hcl" "ecl" "pid"))
(setf test-rec "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm")
(setf test-file "~/adventofcode2020/day04/test-input.txt")

(valid-record-count<-filename test-file)





;;; DEPRECATED

(defun ht-entries<-pairs (ht pairs)
  (mapcar (lambda (pair) (ht-entry<-kvstring ht pair)) pairs)) 			 

(defun list-of-kvps<-file (fn)
  (list-of-kvps<-records (records<-string (read-file fn))))

(defun ht-entry<-kvstring (ht kvstring)
  (destructuring-bind
      (k v)
      (cl-ppcre:split #\: kvstring)
    (setf (gethash k ht) v)))
