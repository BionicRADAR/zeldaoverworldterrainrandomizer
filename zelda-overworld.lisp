;; A program which intends to make procedurally-generated overworld terrain in 
;; the style of Legend of Zelda. It uses a seeded random number generator
;; to allow different people to generate the same map if using the same seed.
;; This file contains the core process, values, and such, and loads
;; other files to handle most of the work.

(defparameter *mapwidth* 16)
(defparameter *mapheight* 8)
(defparameter *screenwidth* 16)
(defparameter *screenheight* 11)

(defun interval (x y)
  (if (< x y)
	(loop for i from x to y collect i)
	(loop for i from x downto y collect i)))

(load "mt19937-1.1.1/mt19937.lisp")
(load "q1-overworld-reader")
(load "freq-list")
(load "min-priority-queue")
(load "place-biomes")
(load "zelda-edges")
(load "alter-columns")
(load "zelda-screens")
(load "adapt-screens")
(load "zelda-smoother")
(load "palettes")
(load "rom-writer")
(load "zeldadisplay/zelda-display")
(load "dump-map")

(defun init-rand (seed)
	(mt19937:set-random-seed seed))

(define-condition invalid-random-maximum-error (error)
  ((maximum :initarg :maximum :reader maximum)))


(defun mrandom (maximum)
  (if (< maximum 1)
	(error 'invalid-random-maximum-error :maximum maximum)
	(mt19937:random maximum)))


(defun format-gen-screen (screen)
  (format t " ~{~<~% ~,35:;~2,'0x ~>~}" (apply #'append screen)))

(defun deep-copy-list (lst)
  (labels ((copy-one-element (x)
							 (if (listp x)
							   (deep-copy-list x)
							   x)))
	(mapcar #'copy-one-element lst)))

(defun valid-screen (overworld-map y x)
  (and (> y -1) (< y (array-dimension overworld-map 0)) 
	   (> x -1) (< x (array-dimension overworld-map 1))))

(defun construct-overworld (width height)
;	put init step here
  (let ((biomes (make-biomes width height)))
	(setf *biomes* biomes)
	(run-edge-adder biomes *biome-coords-list*)
	(format-edges-and-biomes *horizontal-edges* *vertical-edges* biomes)
	(alter-columns biomes *horizontal-edges* *vertical-edges*)
	(place-screens width height biomes *special-biome-identifiers*
				   *horizontal-edges* *vertical-edges*)
	(adapt-screens width height biomes *horizontal-edges* *vertical-edges* 
				   *new-screens* *new-base-screens* *spec-screens-lists*
				   *special-biome-identifiers*)
	(smooth-map *new-screens* *new-base-screens* biomes *horizontal-edges*
				*vertical-edges*)
	(make-palettes width height biomes *new-base-screens*
				   (cadr (assoc 'grave *biome-coords-list*)))))

(defun copy-new-screens (new-screens copy-loc)
  (mapc (lambda (x)
		  (set-arr2d copy-loc x (copy-seq (get-arr2d new-screens x))))
		(get-coords-list (array-dimension new-screens 1)
						 (array-dimension new-screens 0))))
	
(defun print-map (width height)
  (filter-screens-to-col-num *new-screens*)
  (make-png "test-map" (interval 0 127)
			(coerce (mapcar (lambda (x) 
							  (if (numberp x) 
								(aref +init-duplicate-screens+ x) x))
							(mapcar (lambda (x)
									  (aref *new-screens* (car x) (cadr x)))
									(get-coords-list width height)))
					'array)
			#'new-column-by-number
			(mapcar (lambda (x) 
					  (get-arr2d *palettes* x))
					(get-coords-list width height))
			(mapcar (lambda (x) nil) (interval 0 127))
			16 8 16 11))

(defun shuffle-list (lst)
  (let ((lst-array (coerce lst 'array)))
	(mapc (lambda (x)
			(let* ((loc (mrandom x))
				   (val (aref lst-array loc)))
			  (setf (aref lst-array loc) (aref lst-array (1- x)))
			  (setf (aref lst-array (1- x)) val)))
		  (interval (length lst) 1))
	(coerce lst-array 'list)))

(defun alter-rom ())

(defun output-map-data ())

(defun randomize ()
  (handler-case (construct-overworld *mapwidth* *mapheight*)
	('invalid-random-maximum-error () (randomize))))

;(defmacro pop-nth (lst n)
;  (multiple-value-bind (dummies vals new setter getter)
;	(get-setf-expansion lst)
;	(let ((n-gen (gensym)))
;	  `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter) (,n-gen ,n))
;		 (if (zerop ,n-gen)
;		   (progn
;			 (when ,(cdr new) (error "Can't expand this."))
;			 (prog1 (car ,(car new)) 
;					(setq ,(car new) (cdr ,(car new)))
;					,setter))
;		   (pop (cdr (nthcdr (1- ,n-gen) ,getter))))))))

(defmacro pop-nth (lst n)
  (let ((n-gen (gensym)))
	`(let ((,n-gen ,n))
	   (if (zerop ,n-gen)
		 (pop ,lst)
		 (pop (cdr (nthcdr (1- ,n-gen) ,lst)))))))
