;; -----------------------------------------------------------------------------
;; Day 1 -- part 1

(defun read-file (filepath)
  "Read a file returning lines as a list"
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
       while line collect line)))

(defun str->num (s)
  "Parse a string as an integer"
  (parse-integer (string s)))

(defun read-report (filepath)
  "Read the report format (integer per line)"
  (mapcar #'str->num (read-file filepath)))

(defun main-day-1-1 ()
  (let ((depths (read-report "input"))
	(increases 0))
    (reduce #'(lambda (x y) (when (< x y) (incf increases)) y)
	    depths
	    :start 1
	    :initial-value (car depths))
    increases))

;; -----------------------------------------------------------------------------
;; Day 1 -- part 2

(defparameter *filepath* "./day1/input")
(defparameter *window-size* 3)
(defparameter *num-windows* 4)
(defparameter *window-stride* 1)

(defun 1d-convolve (input win-size win-stride)
  (loop for i from 0 upto (- (length input) win-size) by win-stride
	collect (apply #'+ (subseq input i (+ i 3)))))

(defun calculate-increases (measurements)
  (let ((increases 0))
    (reduce #'(lambda (x y) (when (< x y) (incf increases)) y)
	    measurements
	    :start 1
	    :initial-value (car measurements))
    increases))

(defun main-day-1-2 ()
  (calculate-increases
   (1d-convolve (read-report *filepath*) *window-size* *window-stride*)))

;; -----------------------------------------------------------------------------
;; Day 2 -- part 1

(defparameter *position* (list :horizontal 0 :depth 0))
(defparameter *allowed-functions* '("FORWARD" "DOWN" "UP"))

(defun forward (amt) (incf (getf *position* :horizontal) amt))
(defun down (amt) (incf (getf *position* :depth) amt))
(defun up (amt) (down (- amt)))

(defun str->function (str)
  (symbol-function (find-symbol (string-upcase str))))

(defun function->str (function)
  (symbol-name (nth-value 2 (function-lambda-expression function))))

(defun is-allowed-function-p (function)
  (member (string-upcase (function->str function)) *allowed-functions* :test #'equal))

(defun parse-command (command-line)
  (let* ((command-line (coerce command-line 'list))
	 (sep (position #\  command-line))
	 (cmd (str->function (coerce (subseq command-line 0 sep) 'string)))
	 (amt (str->num (coerce (subseq command-line (+ sep 1)) 'string))))
    (list cmd amt)))

(defun read-commands (filepath)
  (mapcar #'parse-command (read-file filepath)))

(defun run-course (commands)
  (mapc #'(lambda (cmd)
	      (let ((cmd (first cmd))
		    (amt (second cmd)))
		(unless (is-allowed-function-p cmd)
		  (error "Trying to execute bad command: ~A" cmd))
		(funcall cmd amt)))
	  commands))

(defun main-day-2-1 ()
  (let ((filepath "./day2/input"))
    (run-course (read-commands filepath))
    (* (getf *position* :horizontal)
       (getf *position* :depth))))


;; -----------------------------------------------------------------------------
;; Day 2 -- part 2

(defparameter *position* (list :horizontal 0 :depth 0 :aim 0))

(defmacro mkcmd (fun keyword amt)
  `(,fun (getf *position* ,keyword) ,amt))

(defun down (amt) (mkcmd incf :aim amt))
(defun up (amt) (mkcmd decf :aim amt))
(defun forward (amt)
  (mkcmd incf :horizontal amt)
  (mkcmd incf :depth (* amt (getf *position* :aim))))

(defun final-position ()
  (* (getf *position* :horizontal)
     (getf *position* :depth)))

(defun main-day-2-2 ()
  (run-course (read-commands "day2/input"))
  (final-position))

;; -----------------------------------------------------------------------------
;; Day 3 -- part 1

(defun power-consumption (gamma-rate epsilon-rate)
  (* gamma-rate epsilon-rate))

(defun most-common-bit (seq)
  (let ((ones (count 1 seq))
	(zeros (count 0 seq)))
    (cond
      ((< ones zeros) 0)
      ((> ones zeros) 1)
      (t nil))))

(defun get-gamma-value (seq)
  (most-common-bit seq))

(defun get-epsilon-value (seq)
  (abs (1- (most-common-bit seq))))

(defun get-gamma (binary)
  (binary->num (mapcar #'get-gamma-value binary)))

(defun get-epsilon (binary)
  (binary->num (mapcar #'get-epsilon-value binary)))

(defun str->list (str) (coerce str 'list))
(defun list->str (lst) (apply #'concatenate 'string (mapcar #'write-to-string lst)))
(defun binary->num (lst) (parse-integer (list->str lst) :radix 2))

(defun transpose (lst)
  (loop for j from 0 below (length (car lst))
	collect (mapcar #'(lambda (line) (nth j line)) lst)))

(defun read-binary-file (filepath)
  (let ((contents (mapcar #'str->list (read-file filepath))))
    (mapcar #'(lambda (lst) (mapcar #'str->num lst)) (transpose contents))))

(defun main-day-3-1 ()
  (let* ((filepath "./day3/input")
	 (binary (read-binary-file filepath))
	 (gamma-rate (get-gamma binary))
	 (epsilon-rate (get-epsilon binary)))
    (power-consumption gamma-rate epsilon-rate)))

;; -----------------------------------------------------------------------------
;; Day 3 -- part 2

(defun has-in-pos-p (lst n idx)
  (= (nth idx lst) n))

(defun filter-lists (lsts n idx)
  (remove-if-not #'(lambda (seq) (has-in-pos-p seq n idx)) lsts))

(defun filter-ratings (binary eql-val filter-fn)
  (let ((seqs (transpose binary)))
    (loop for idx from 0 below (length (car binary))
	  for cm = (funcall filter-fn (nth idx (transpose seqs)))
	  when (= 1 (length seqs))
	    return (first seqs)
	  if (eq nil cm)
	    do (setf seqs (filter-lists seqs eql-val idx))
	  else
	    do (setf seqs (filter-lists seqs cm idx)))))

(defun most-common (seq) (most-common-bit seq))
(defun least-common (seq)
  (let ((mc (most-common-bit seq)))
    (when mc
      (abs (1- mc)))))

(defun oxygen-generator-rating (binary)
  (nth-value 0 (binary->num (filter-ratings binary 1 #'most-common))))

(defun co2-scrubber-rating (binary)
  (nth-value 0 (binary->num (filter-ratings binary 0 #'least-common))))

(defun main-day-3-2 ()
  (let* ((filepath "./day3/input")
	 (binary (read-binary-file filepath))
	 (oxygen (oxygen-generator-rating binary))
	 (co2 (co2-scrubber-rating binary)))
    (* oxygen co2)))

;; -----------------------------------------------------------------------------
;; Day 4 -- part 1

(defun split (str delim)
  (let ((pos (position delim str :test #'string-equal)))
    (if pos
	(let ((item (subseq str 0 pos))
	      (str (subseq str (1+ pos))))
	  (if str
	      (cons item (split str delim))
	      item))
	(cons str nil))))

(defun get-row (n bingo-card)
  (nth n bingo-card))

(defun get-col (n bingo-card)
  (nth n (transpose bingo-card)))

(defun is-empty-str-p (s)
  (equal s ""))

(defun read-row (line delim)
  (mapcar
   #'(lambda (row) (mapcar #'str->num (remove-if #'is-empty-str-p (split row delim))))
   line))

(defun read-bingo-system (filepath)
  (let* ((contents (remove-if #'is-empty-str-p (read-file filepath)))
	 (draws (mapcar #'str->num (split (first contents) ",")))
	 (cards (mapcar #'(lambda (line) (read-row line " "))
			(loop for i from 0 below (length (rest contents)) by 5
			      collect (subseq (rest contents) i (+ i 5))))))
    (values draws cards)))

(defun is-seq-filled-p (draws seq)
  (eq nil (set-difference seq draws)))

(defun is-winner-p (draws card)
  (dotimes (idx (length card))
    (when (or (is-seq-filled-p draws (get-col idx card))
	      (is-seq-filled-p draws (get-row idx card)))
      (return-from is-winner-p t))))

(defun get-missing (drawn card)
  (remove-if #'null (mapcar #'(lambda (row) (set-difference row drawn)) card)))

(defun flatten (lst)
  (if (null lst)
      lst
      (if (atom (first lst))
	  (cons (first lst) (flatten (rest lst)))
	  (append (flatten (first lst)) (flatten (rest lst))))))

(defun count-missing (drawn card)
  (length (flatten (get-missing drawn card))))

(defun board-score (drawn card)
  (let ((s (apply #'+ (flatten (get-missing drawn card)))))
    (* (first (last drawn)) s)))

(defun main-day-4-1 ()
  (let* ((filepath "./day4/input"))
    (multiple-value-bind (draws cards) (read-bingo-system filepath)
      (loop for draw in draws collecting draw into drawn
	    when (position t (mapcar #'(lambda (c) (is-winner-p drawn c)) cards))
	      do (return (board-score
		   drawn
		   (nth (position t (mapcar #'(lambda (c) (is-winner-p drawn c)) cards)) cards)))))))
