;; A program which reads in data from a full q1 map with each tile 
;; labeled in hex
;; Source: https://raw.githubusercontent.com/asweigart/nes_zelda_map_data/master/overworld_map/nes_zelda_overworld_tile_map.txt

(defparameter *map-width* 16)
(defparameter *map-height* 8)
(defparameter *screen-width* 16)
(defparameter *screen-height* 11)
(defparameter *screens* (make-array (list *map-width* *map-height*)))
(defparameter *columns* ())
(defparameter *acolumns* #())
(defparameter *eql-values* (make-hash-table))
(defparameter *adj-columns* ())


(defun init-screen ()
  (make-array (list *screen-width* *screen-height*)))

(defun init-map ()
  (setf *screens* (make-array (list *map-width* *map-height*)))
  (loop for x below *map-width* do
		(loop for y below *map-height* do
			  (setf (aref *screens* x y) (init-screen)))))

(defun make-hex-int (raw)
  (parse-integer (if (numberp raw) 
				   (write-to-string raw)
				   (symbol-name raw)) :radix 16))

(defun fill-screens (fname)
  (init-map)
  (with-open-file (raw fname :direction :input)
	(loop for y below (* *screen-height* *map-height*) do
	  (multiple-value-bind (screen-y tile-y) (floor y *screen-height*)
	    (loop for x below (* *screen-width* *map-width*) do
		  (multiple-value-bind (screen-x tile-x) (floor x *screen-width*)
			(setf (aref (aref *screens* screen-x screen-y) tile-x tile-y)
						(make-hex-int (read raw)))))))))

(fill-screens "q1-overworld-raw.txt")

(defun digit-to-letter (digit)
  (if (> digit 9)
	"?"
	(coerce (list (code-char (+ (char-code (digit-char digit)) 17))) 'string)))

(defun format-screen (x y)
  (let ((screen (aref *screens* x y))
		(screen-name (concatenate 'string 
								  (digit-to-letter y) 
								  (write-to-string (1+ x)))))
	(format t "~3@a ~{~2,'0d ~}~&" screen-name 
			(loop for x below 16 collect x))
	(format t " ~{~<~% ~,52:;~2,'0x ~>~}"
			(loop for y below *screen-height* append (cons y
			  (loop for x below *screen-width* collect (aref screen x y)))))))

(defun fill-eql-table (fname)
  (with-open-file (listdata fname :direction :input)
	(mapc (lambda (lst)
			 (let ((val (car lst))
				   (keys (cadr lst)))
			   (mapc (lambda (key)
						(setf (gethash key *eql-values*) val))
					  keys)))
		   (read listdata))))

(defun sub-equals ()
  (fill-eql-table "equal-tile-list.txt")
  (loop for screen-x below *map-width* do
	(loop for screen-y below *map-height* do
	  (loop for tile-x below *screen-width* do
		(loop for tile-y below *screen-height* do
		  (let ((repl (gethash (aref (aref *screens* screen-x screen-y)
									 tile-x tile-y) *eql-values*)))
			(when repl
			  (setf (aref (aref *screens* screen-x screen-y)
						  tile-x tile-y) repl))))))))

(defun fill-columns ()
  (sub-equals)
  (loop for screen-x below *map-width* do
	(loop for screen-y below *map-height* do
	  (loop for tile-x below *screen-width* do
		(let ((col (loop for tile-y below *screen-height* collect
						 (aref (aref *screens* screen-x screen-y)
							   tile-x tile-y))))
		  (when (not (member col *columns* :test #'equal))
			(push col *columns*))))))
  (let ((numcols (length *columns*))) 
		(setf *adj-columns* (make-array (1+ numcols)))
		(loop for x below (length *adj-columns*) do
			  (setf (aref *adj-columns* x) (make-hash-table))))
  (setf *acolumns* (coerce (cons nil *columns*) 'array)))

(defun format-columns (column-list)
  (princ " 01 02 03 04 05 06 07 08 09 10 11")
  (fresh-line)
  (format t " ~{~<~% ~,34:;~2,'0x ~>~}"
	(apply #'append column-list)))

(defun find-index (arr item)
  (loop for x below (length arr)
		when (equal item (aref arr x))
		return x))

(defun column-numbers ()
  (fill-columns)  
  (append
  (loop for screen-y below *map-height* append
	(loop for screen-x below *map-width* append
	  (loop for tile-x below *screen-width* append
		(let ((col-num (find-index *acolumns*
			(loop for tile-y below *screen-height* collect
					(aref (aref *screens* screen-x screen-y) tile-x tile-y)))))
		  (if (zerop tile-x)
			(list 0 col-num) 
			(list col-num))))))
    '(0)))

(defun incr-hash (key hash)
  (if (gethash key hash)
	(setf (gethash key hash) (1+ (gethash key hash)))
	(setf (gethash key hash) 1)))

(defun count-hash (hash)
  (setf (gethash 'total hash)
		(loop for x being each hash-value of hash sum x)))

(defun fill-adj-columns ()
  (let ((col-nums (column-numbers)))
	(incr-hash (cadr col-nums) (aref *adj-columns* (car col-nums)))
	(labels ((f (lst)
	  (let ((left (car lst))
			(num (cadr lst))
			(right (caddr lst)))
		(when (not (zerop left))
		  (incr-hash left (aref *adj-columns* num)))
		(when right
		  (when (not (zerop right))
			(incr-hash right (aref *adj-columns* num)))
		  (f (cdr lst))))))
	  (f col-nums))
    (mapc #'count-hash (coerce *adj-columns* 'list))))
