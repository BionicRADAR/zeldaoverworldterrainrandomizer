;; A part of a program which intends to make a procedurally-generated overworld in the 
;; style of Legend of Zelda. It uses a seeded random number generator
;; to allow different people to generate the same map if using the same seed.
;; This piece places the biomes on the overworld map and is the first step in
;; constructing an overworld.
;; Author: Nathan Schleicher


(defparameter *biomes* (make-array (list *mapheight* *mapwidth*)))
(defparameter *markers* (make-array (list *mapheight* *mapwidth*)))
(defparameter *base-lake-offsets* '((0 2) (0 3) (0 4) (1 0) (1 1) (1 2) 
									(1 3 island) (1 4) (2 0) (2 1 island)
									(2 2) (2 3) (2 4) (3 0) (3 1) (3 2)))
(defparameter *base-graveyard-offsets* '(3 2))
(defparameter *biome-size-alist* '((mountain 30) (coast 18) (big-lake 16)
								   (small-lake 4) (island 3) (graveyard 6)
								   (desert 5) (forest 11) (woods 9)
								   (hills 8)))
(defconstant +biome-priority+ '(a-fairy pond open-ruin island spring big-lake 
										small-lake river-in graveyard desert
										coast hills mountain forest woods 
										river-out))
(defparameter *biome-coords-list* nil)
(defparameter *special-biome-identifiers* nil)

;9 total ruins; 3 are currently islands
(defparameter *map-features* '((open-ruin 6) (pond 1) (a-fairy 2)))
;distances to attempt to avoid having features too close together
;the exact numbers here are somewhat arbitrary
(defparameter *feature-distances* '((open-ruin 2) (pond 2) (a-fairy 6)))

(defun reset-map ()
  (setf *biomes* (make-array (list *mapheight* *mapwidth*)))
  (setf *markers* (make-array (list *mapheight* *mapwidth*)))
  (setf *map-features* '(copy-seq ((open-ruin 6) (pond 1) (a-fairy 2))))
  (setf *biome-coords-list* nil)
  (setf *special-biome-identifiers* nil))

(defun next-rotate-pos (pos width height)
  (cond ((eql (cadr pos) (1- width)) (if (eql (car pos) (1- height))
								  (list (car pos) (1- (cadr pos)))
								  (list (1+ (car pos)) (cadr pos))))
		((eql (cadr pos) 0) (if (eql (car pos) 0)
							  (list (car pos) (1+ (cadr pos)))
							  (list (1- (car pos)) (cadr pos))))
		((eql (car pos) (1- height)) (list (car pos) (1- (cadr pos))))
		(t (list (car pos) (1+ (cadr pos))))))

(defun seek-pos (num width height)
  (let ((modnum (mod num (- (* 2 (+ width height)) 4))))
	 (if (< modnum width)
	   (list 0 modnum)
	   (if (< modnum (1- (+ width height)))
		 (list (1+ (- modnum width)) (1- width))
		 (if (< modnum (- (+ width width height) 2))
		   (list (1- height) (- (+ width width height) (+ 3 modnum)))
		   (list (- (+ width width height height) (+ 4 modnum)) 0))))))

(defun coast-edge (start-pos edge-length width height)
  (loop repeat edge-length
		with pos = start-pos
		collect pos
		do (setf pos (next-rotate-pos pos width height))))

(defun coast-cond (coast-edge-list width height)
  (cond ((and (find '(0 0) coast-edge-list :test #'equal)
			   (find '(0 1) coast-edge-list :test #'equal)
			   (find '(1 0) coast-edge-list :test #'equal))
		   'top-left)
		((and (find `(0 ,(1- width)) coast-edge-list :test #'equal)
		      (find `(1 ,(1- width)) coast-edge-list :test #'equal)
			  (find `(0 ,(- width 2)) coast-edge-list :test #'equal))
		   'top-right)
		((and (find `(,(1- height) 0) coast-edge-list :test #'equal)
		      (find `(,(- height 2) 0) coast-edge-list :test #'equal)
			  (find `(,(1- height) 1) coast-edge-list :test #'equal))
		   'bottom-left)
		((and (find `(,(1- height) ,(1- width)) coast-edge-list :test #'equal)
		      (find `(,(- height 2) ,(1- width)) coast-edge-list :test #'equal)
			  (find `(,(1- height) ,(- width 2)) coast-edge-list :test #'equal))
		   'bottom-right)
		((eql (caar coast-edge-list) 0) 'top)
		((eql (caar coast-edge-list) (1- height)) 'bottom)
		((eql (cadar coast-edge-list) 0) 'left)
		(t 'right)))

(defun coast-cluster-side (coast-edge loc vert horiz overworld-map)
  (princ 'side-cluster) (fresh-line)
  (if (or (eq loc 'left) (eq loc 'right))
	(error "unimplemented sides for coast cluster")
	(let* ((start-pos (nth (1+ (mrandom (- (length coast-edge) 4))) coast-edge))
		   (start-v (car start-pos))
		   (start-h (cadr start-pos))
		   (horiz (if (eq loc 'top)
				   (lambda (x) (+ start-h x))
				   (lambda (x) (- start-h x)))))
	  (if (eq loc 'top)
		(push 'island (aref overworld-map (funcall vert 0) (funcall horiz 1)))
		(push 'island (aref overworld-map (funcall vert 2) (funcall horiz 1))))
	  (push `(coast-cluster ,loc nil ,vert ,horiz)
			*special-biome-identifiers*)
	  (append coast-edge
			  `((,(funcall vert 1) ,(funcall horiz 0))
			    (,(funcall vert 1) ,(funcall horiz 1))
			    (,(funcall vert 1) ,(funcall horiz 2))
			    (,(funcall vert 2) ,(funcall horiz 1))
				(,(funcall vert 2)
				  ,(if (zerop (mrandom 2))
					 (funcall horiz 0)
					 (funcall horiz 2))))))))

(defun coast-horiz-corner (coast-edge vert horiz overworld-map)
  (princ 'horiz-corner) (fresh-line)
  (push 'island (aref overworld-map 
					  (funcall vert (if (zerop (funcall vert 0)) 0 2))
					  (funcall horiz 2)))
  (append coast-edge
		`((,(funcall vert 1) ,(funcall horiz 1))
		  (,(funcall vert 2) ,(funcall horiz 1))
		  (,(funcall vert 1) ,(funcall horiz 2))
		  (,(funcall vert 2) ,(funcall horiz 2))
		  (,(funcall vert 1) ,(funcall horiz 3)))))

(defun coast-vert-corner (coast-edge vert horiz overworld-map)
  (princ 'vert-corner) (fresh-line)
  (push 'island (aref overworld-map (funcall vert 2) (funcall horiz 0)))
  (append coast-edge
		`((,(funcall vert 1) ,(funcall horiz 1))
		  (,(funcall vert 2) ,(funcall horiz 1))
		  (,(funcall vert 1) ,(funcall horiz 2))
		  (,(funcall vert 2) ,(funcall horiz 2))
		  (,(funcall vert 3) ,(funcall horiz 1)))))

(defun coast-cluster (coast-edge width height overworld-map)
  (let* ((loc (coast-cond coast-edge width height))
		(vert
		  (if (or (eq loc 'top-left) (eq loc 'top) 
				  (eq loc 'top-right) (eq loc 'right))
			(lambda (x) x)
			(lambda (x) (- height (1+ x)))))
		(horiz
		  (if (or (eq loc 'top-left) (eq loc 'left) 
				  (eq loc 'bottom-left) (eq loc 'top))
			(lambda (x) x)
			(lambda (x) (- width (1+ x)))))
		(spec-biome-id (lambda (loc dir) 
						 `(coast-cluster ,loc ,dir ,vert ,horiz))))
	(if (or (eq loc 'top) (eq loc 'left) (eq loc 'right) (eq loc 'bottom))
	  (coast-cluster-side coast-edge loc vert horiz overworld-map)
	  (cond ((not (find `(,(funcall vert 4) ,(funcall horiz 0))
						coast-edge :test #'equal))
			 (push (funcall spec-biome-id loc 'horizontal)
							*special-biome-identifiers*)
			 (coast-horiz-corner coast-edge vert horiz overworld-map))
			((not (find `(,(funcall vert 0) ,(funcall horiz 4))
						coast-edge :test #'equal))
			 (push (funcall spec-biome-id loc 'vertical)
							*special-biome-identifiers*)
			 (coast-vert-corner coast-edge vert horiz overworld-map))
			(t (if (zerop (mrandom 2))
				 (progn
				   (push (funcall spec-biome-id loc 'horizontal)
						 *special-biome-identifiers*)
				   (coast-horiz-corner coast-edge vert horiz overworld-map))
				 (progn
				   (push (funcall spec-biome-id loc 'vertical)
						 *special-biome-identifiers*)
				   (coast-vert-corner coast-edge vert horiz overworld-map))))))))
;Items in coast-cluster *special-biome-identifiers*:
;location: one of top-left top top-right left right bottom-left bottom bottom-right
;direction: only for corner coast clusters, either vertical or horizontal (depending on which
;	way the cluster stretches); nil if not in a corner.
;vert: function taking a value and giving a vertical coordinate that distance away from the 
;	vertical map edge (ie, the top or bottom edge) that the cluster is on.
;horiz: function taking a value and giving a horizontal coordinate that distance away from the
;	horizontal map edge (ie, the left or right edge) that the cluster is on; 
;	or, if the cluster is only on a vertical edge, horiz returns a horizontal distance from
;	an "origin point" on the cluster into the cluster.


(defun place-coast (width height edge-length overworld-map)
  (let ((coast (coast-cluster 
				 (coast-edge 
				   (seek-pos (mrandom (- (* 2 (+ width height)) 4)) width height)
			       edge-length width height)
				 width height overworld-map)))
	(push `(coast-edge ,(subseq coast 0 13)) *biome-coords-list*)
	(push `(coast-cluster ,(subseq coast 13)) *biome-coords-list*)
	(mapc
	  (lambda (x)
		(push 'coast (aref overworld-map (car x) (cadr x))))
	  coast)))

(defun positions-empty (overworld-map positions)
  (every (lambda (x) (not (aref overworld-map (car x) (cadr x)))) positions))

(defun place-big-lake (width height overworld-map lake-offsets)
  (let* ((rotation (mrandom 4))
		 (offset-mutators
		   `(,(lambda (x) x) 
			 ,(lambda (x) (append (list (car x) (- 4 (cadr x))) (cddr x)))
			 ,(lambda (x) (append (list (cadr x) (car x)) (cddr x)))
			 ,(lambda (x) (append (list (- 4 (cadr x)) (car x)) (cddr x)))))
		 (mutated-offsets 
		   (mapcar (nth rotation offset-mutators) lake-offsets))
		 (origin `(,(mrandom (- height (apply #'max 
										(mapcar #'car mutated-offsets)))) 
				   ,(mrandom (- width (apply #'max 
										(mapcar #'cadr mutated-offsets))))))
		 (positions (mapcar 
					  (lambda (x) (append 
									(list (+ (car origin) (car x))
										  (+ (cadr origin) (cadr x)))
									(cddr x)))
					  mutated-offsets)))
	(if (positions-empty overworld-map positions)
	  (progn (push `(big-lake ,origin ,rotation) *special-biome-identifiers*)
			 (push `(big-lake ,positions) *biome-coords-list*)
	  (mapcar (lambda (x)
			   (push 'big-lake (aref overworld-map (car x) (cadr x)))
			   (when (cddr x)
				 (push (caddr x) (aref overworld-map (car x) (cadr x)))))
			  positions))
	  (place-big-lake width height overworld-map lake-offsets))))

(defun place-graveyard (width height overworld-map graveyard-dims)
  (let* ((dir (mrandom 2))
		 (mutated-dims (if (zerop dir)
						 graveyard-dims
						 (list (cadr graveyard-dims) (car graveyard-dims))))
		 (origin (list (mrandom (- height (1- (car mutated-dims))))
				       (mrandom (- width (1- (cadr mutated-dims))))))
		 (positions
	(loop for x from (car origin) to (1- (+ (car mutated-dims) (car origin)))
		  append (loop for y from (cadr origin) 
				         to (1- (+ (cadr mutated-dims) (cadr origin)))
				   collect (list x y)))))
	(if (positions-empty overworld-map positions)
	  (progn (push `(grave ,origin ,dir) *special-biome-identifiers*)
			 (push `(grave ,positions) *biome-coords-list*)
	  (mapcar (lambda (x)
				(push 'graveyard (aref overworld-map (car x) (cadr x))))
			  positions))
	  (place-graveyard width height overworld-map graveyard-dims))))

(defun offshoot-test (width height overworld-map origin)
	(append (when (> (car origin) 0) '((-1 0) (-1 1)))
				   (when (> (cadr origin) 0) '((0 -1) (1 -1)))
				   (when (< (car origin) (- height 2)) '((2 0) (2 1)))
				   (when (< (cadr origin) (- width 2)) '((0 2) (1 2)))))


(defun place-desert (width height overworld-map)
  (let* ((origin (list (mrandom (- height 2))
					   (mrandom (- width 2))))
		 (offshoot-options
		   (append (when (> (car origin) 0) '((-1 0) (-1 1)))
				   (when (> (cadr origin) 0) '((0 -1) (1 -1)))
				   (when (< (car origin) (- height 2)) '((2 0) (2 1)))
				   (when (< (cadr origin) (- width 2)) '((0 2) (1 2)))))
		 (offshoot (nth (mrandom (length offshoot-options)) offshoot-options))
		 (positions (mapcar (lambda (x) (list (+ (car origin) (car x))
											  (+ (cadr origin) (cadr x))))
							`(,offshoot (0 0) (0 1) (1 0) (1 1)))))
	(if (positions-empty overworld-map positions)
	  (progn
		(mapcar (lambda (x)
				  (push 'desert (aref overworld-map (car x) (cadr x))))
				positions)
		(push `(desert ,positions) *biome-coords-list*)
		(push `(desert ,origin ,offshoot) *special-biome-identifiers*))
	  (place-desert width height overworld-map))))
;pushed to special biome identifiers for desert, in order: origin position and relative position of
;offshoot

(defun place-small-lake (width height overworld-map)
  (let* ((origin (list (mrandom (1- height)) (mrandom (1- width))))
		 (positions `(,origin (,(car origin) ,(1+ (cadr origin)))
					   (,(1+ (car origin)) ,(cadr origin)) 
					   (,(1+ (car origin)) ,(1+ (cadr origin))))))
	(if (every (lambda (x) (not (aref overworld-map (car x) (cadr x))))
			   positions)
	  (progn (push `(small-lake ,origin) *special-biome-identifiers*)
			 (push `(small-lake ,positions) *biome-coords-list*)
			 (mapcar (lambda (x)
					   (push 'small-lake (aref overworld-map (car x) (cadr x))))
					 positions))
	  (place-small-lake width height overworld-map))))

(defun can-combine (biome)
  (every (lambda (x)
		   (every (lambda (y) (not (eq x y))) '(coast island desert graveyard)))
		 biome))

(defun zelda-dfs (width height pos condit action)
  (funcall action (car pos) (cadr pos))
  (mapc (lambda (x)
		  (let ((new-pos (list (+ (car pos) (car x)) (+ (cadr pos) (cadr x)))))
			(unless (or (>= (car new-pos) height) (>= (cadr new-pos) width)
						(< (car new-pos) 0) (< (cadr new-pos) 0)
						(funcall condit (car new-pos) (cadr new-pos)))
			  (zelda-dfs width height new-pos condit action))))
		'((-1 0) (0 -1) (0 1) (1 0))))

(defun find-chunks (width height overworld-map markers)
  (loop for y below height
		with mark = 1
		do (loop for x below width do
				 (unless (or (aref overworld-map y x) (aref markers y x))
				   (zelda-dfs width height `(,y ,x) 
							  (lambda (y x) (or (aref overworld-map y x)
											    (aref markers y x)))
							  (lambda (y x) (setf (aref markers y x) mark)))
				   (setf mark (1+ mark))))))

(defun list-biomes (width height overworld-map)
  (let ((already-checked (make-array `(,height ,width))))
  (loop for y below height append
		(loop for x below width append
			  (let ((biome (remove 'river-out (aref overworld-map y x)))
					(biome-screens nil))
				(when (and (not (aref already-checked y x))
						(eql 1 (length biome)) 
						(member (car biome) '(forest hills woods mountain)))
				  (zelda-dfs width height `(,y ,x)
							 (lambda (y x) 
							   (or (not (equal biome 
											   (remove 'river-out 
													   (aref overworld-map y x))))
									(aref already-checked y x)))
							 (lambda (y x) (push `(,y ,x) biome-screens)
							   (setf (aref already-checked y x) t)))
				  `((,biome ,biome-screens))))))))


(defun find-repeated-elements (lst)
  (if lst
	(labels ((this-repeated (lst item num)
							(if (and lst (eql (car lst) item))
							  (this-repeated (cdr lst) item (1+ num))
							  (values lst num item))))
	  (multiple-value-bind (part-lst num item) 
		(this-repeated (cdr lst) (car lst) 1)
		(cons `(,num ,item) (find-repeated-elements part-lst))))
	nil))

(defun sum-before-num (lst num sum)
  (if (or (not lst) (eql (car lst) num))
	sum
	(sum-before-num (cdr lst) num (+ (car lst) sum))))

(defun get-row (arr-2d origin len)
  (loop repeat len
		for x from (cadr origin)
		collect (aref arr-2d (car origin) x)))

(defun get-row-info (arr-2d origin len condit)
  (let* ((row (find-repeated-elements (get-row arr-2d origin len)))
		 (valid-row (mapcar #'car (remove-if (lambda (x) 
											   (not (funcall condit (cadr x))))
											 row)))
		 (max-cont (if valid-row
					 (apply #'max valid-row)
					 0)))
	`(,max-cont 
	  ,(cadr (assoc max-cont row))
	   (,(car origin) 
		 ,(sum-before-num (mapcar #'car row) max-cont (cadr origin))))))

(defun find-longest-contiguous-row (width height markers)
  (let ((row-info (loop for y below height
						collect (get-row-info markers `(,y 0) 
											  width #'identity))))
	(assoc (apply #'max (mapcar #'car row-info)) row-info)))

(defun find-adj-contiguous-row (height markers origin len value)
  (let ((row0 (if (> (car origin) 0) 
				(get-row-info markers `(,(1- (car origin)) ,(cadr origin)) len
							  (lambda (x) (eql x value)))
				'(-1 (-1 -1))))
		(row1 (if (< (car origin) (1- height))
				(get-row-info markers `(,(1+ (car origin)) ,(cadr origin)) len
							  (lambda (x) (eql x value)))
				'(-1 (-1 -1)))))
	(if (< (car row0) (car row1))
	  row1
	  row0)))
	

(defun place-mountains (width height overworld-map markers)
  (let* ((row0 (find-longest-contiguous-row width height markers))
		 (row1 (find-adj-contiguous-row 
				 height *markers* (caddr row0) (car row0) (cadr row0))))
	(flet ((get-coords (row)
					   (loop repeat (car row)
							 for x from (cadr (caddr row))
							 collect `(,(caaddr row) ,x))))
	  (mapcar (lambda (x)
				(push 'mountain (aref overworld-map (car x) (cadr x))))
			  (append (get-coords row0) (get-coords row1))))))

;(defun empty-space (coords overworld-map)
;  (every (lambda (x) (not (aref overworld-map (car x) (cadr x))))))

(defun expand-rectangle (width height overworld-map origin)
  (labels ((empty-space (coords)
						(every (lambda (x) (not (aref overworld-map
											(+ (car origin) (car x))
											(+ (cadr origin) (cadr x)))))
							   coords))
		   (expand-one (dimension)
					   (if (and (< (+ dimension (car origin)) height)
								(< (+ dimension (cadr origin)) width)
							 (empty-space 
						 (cons `(,dimension ,dimension)
							 (loop repeat dimension for x from 0 
								   append `((,dimension ,x) 
											(,x ,dimension))))))
						 (expand-one (1+ dimension))
						 dimension)))
	(expand-one 1)))
	
	
	;(let ((dimension (expand-one 0)))
	;  (loop for x below dimension
	;		append (loop for y below dimension
	;					 collect `(,(+ (car origin) x) 
	;							   ,(+ (cadr origin) y)))))))

(defun get-rectangles (width height overworld-map)
  (loop for x below height append
		(loop for y below width collect
			  (if (aref overworld-map x y)
				`(0 (,x ,y))
				`(,(expand-rectangle width height overworld-map `(,x ,y))
				   (,x ,y))))))

(defun pick-random-max (alist)
  (let* ((max-key (apply #'max (mapcar #'car alist)))
		 (maxes (remove-if (lambda (x) (not (eql (car x) max-key))) alist)))
	(nth (mrandom (length maxes)) maxes)))

(defun place-forest (width height overworld-map max-size typ)
  (let* ((rectangles (get-rectangles width height overworld-map))
		 (max-dim (apply #'max (mapcar #'car rectangles)))
		 (origin-mod (if (> max-dim max-size)
					   `(,(mrandom (- max-dim (1- max-size))) 
						  ,(mrandom (- max-dim (1- max-size))))
					   '(0 0)))
		 (origin-start (cadr (pick-random-max rectangles)))
		 (origin `(,(+ (car origin-start) (car origin-mod))
				   ,(+ (cadr origin-start) (cadr origin-mod))))
		 (final-dim (if (> max-dim max-size) max-size max-dim)))
	(mapc (lambda (x) (push typ (aref overworld-map (car x) (cadr x))))
		  (loop for x below final-dim append
				(loop for y below final-dim collect
					  `(,(+ (car origin) x) ,(+ (cadr origin) y)))))))

(defstruct cell datum next)
(defstruct queue head end)

(defun push-queue (datum queue)
  (let ((cell (make-cell :datum datum)))
	(if (queue-head queue)
	  (progn (setf (cell-next (queue-end queue)) cell)
			 (setf (queue-end queue) cell))
	  (progn (setf (queue-head queue) cell)
			 (setf (queue-end queue) cell)))))

(defun pop-queue (queue)
  (if (queue-head queue)
	(let ((datum (cell-datum (queue-head queue))))
	  (setf (queue-head queue) (cell-next (queue-head queue)))
	  (unless (queue-head queue)
		(setf (queue-end queue) nil))
	  datum)
	nil))

(defun find-neighbors (width height pos)
  (mapcar (lambda (x) `(,(+ (car pos) (car x)) 
						,(+ (cadr pos) (cadr x))))
		  (append
			(when (> (car pos) 0) '((-1 0)))
			(when (> (cadr pos) 0) '((0 -1)))
			(when (< (car pos) (1- height)) '((1 0)))
			(when (< (cadr pos) (1- width)) '((0 1))))))

(defun shuffle (orig-lst)
  (let ((lst (copy-list orig-lst)))
	(loop for x from (length lst) downto 1 collect
		  (pop (subseq lst (mrandom x))))))
	  

(defun rand-bfs (width height start-pos condit)
  (let ((queue (make-queue))
		(marks (make-array `(,height ,width))))
	(setf (aref marks (car start-pos) (cadr start-pos)) t)
	(labels ((bfs (pos lst)
				  (let ((neighbors (remove-if (lambda (x) 
												(aref marks (car x) (cadr x)))
									 (remove-if condit
										(find-neighbors width height pos)))))
					(mapcar (lambda (x) (setf (aref marks (car x) (cadr x)) t))
							neighbors)
					(mapcar (lambda (x) (push-queue x queue)) 
							(shuffle neighbors))
					(if (queue-head queue)
					  (bfs (pop-queue queue) (append lst neighbors))
					  lst))))
	  (bfs start-pos (list start-pos)))))

(defun place-hills (width height overworld-map markers)
  (let* ((mark-counts (count-elements width height markers))
		 (mark-choice (pick-random-max mark-counts))
		 (positions 
		   (loop for y below height append
				 (loop for x below width
					   when (eql (aref markers y x) (cadr mark-choice))
					   collect `(,y ,x)))))
	(mapcar (lambda (x) (push 'hills (aref overworld-map (car x) (cadr x))))
			(if (> (car mark-choice) (cadr (assoc 'hills *biome-size-alist*)))
			  (subseq
				(rand-bfs width height 
						  (nth (mrandom (length positions)) positions)
						  (lambda (x) 
							(not (eql (cadr mark-choice) 
									  (aref markers (car x) (cadr x))))))
				0 (cadr (assoc 'hills *biome-size-alist*)))
			  positions))))


(defun count-elements (width height arr-2d)
  (let* ((element-list nil)
		 (tally 
		   (loop for y below height append
				 (loop for x below width when (aref arr-2d y x)
					   collect (aref arr-2d y x)
					   do (and (aref arr-2d y x)
							   (not (find (aref arr-2d y x) element-list
										  :test #'equal))
							   (push (aref arr-2d y x) element-list))))))
	(mapcar (lambda (x)
			  (list (count x tally :test #'equal) x))
			element-list)))

(defun collect-positions (width height overworld-map biome)
  (loop for y below height append
		(loop for x below width 
			  when (find biome (aref overworld-map y x))
			  collect `(,y ,x))))

(defun collect-neighbors (width height locs)
  (labels ((handle-pos (input output)
				(if input
				  (handle-pos (cdr input)
							  (append (remove-if (lambda (x) 
												   (or (find x output
														 :test #'equal)
													   (find x locs
														 :test #'equal)))
									  (find-neighbors width height (car input)))
									output))
				  output)))
	(handle-pos locs nil)))

(defun expand-small-biome (width height overworld-map biome biome-size)
  (labels ((combinable (biome-lst)
					   (not biome-lst))
;					   (every (lambda (x) (or (eq x 'small-lake)
;											  (eq x 'big-lake))) biome-lst))
		   (expand-one (cur-locs neighbors)
				(if (and neighbors (< (length cur-locs) biome-size))
				  (let ((rand-choice (nth (mrandom (length neighbors)) 
										  neighbors)))
					(push biome (aref overworld-map (car rand-choice)
								(cadr rand-choice)))
					(expand-one (cons rand-choice cur-locs)
								(append (remove-if 
										  (lambda (x)
											(or (find x neighbors :test #'equal)
												(find x cur-locs :test #'equal)
												(not (combinable
													   (aref overworld-map
															 (car x) 
															 (cadr x))))))
										  (find-neighbors width 
														  height rand-choice))
										(remove rand-choice neighbors))))
				  cur-locs)))
	(let ((locs (collect-positions width height overworld-map biome)))
	  (expand-one locs (remove-if (lambda (x) 
									(not (combinable (aref overworld-map
														   (car x) (cadr x)))))
								  (collect-neighbors width height locs))))))
				  
(defun expand-small-biomes (width height overworld-map biome-sizes)
  (expand-small-biome width height overworld-map 'woods 
					  (cadr (assoc 'woods biome-sizes)))
  (expand-small-biome width height overworld-map 'forest
					  (cadr (assoc 'forest biome-sizes)))
  (expand-small-biome width height overworld-map 'mountain
					  (cadr (assoc 'mountain biome-sizes))))


(defun place-output-river (width height overworld-map output-locs)
  (if (< (caar output-locs) (- (1- height) (caadr output-locs)))
	(progn (loop for y from (caar output-locs) downto 0
		  unless (find 'island (aref overworld-map y (cadar output-locs)))
		  do (push 'river-out (aref overworld-map y (cadar output-locs))))
		   (push `(river-out ,(car output-locs) up) *special-biome-identifiers*)
		   (car output-locs))
	(progn (loop for y from (caadr output-locs) to (1- height)
		  unless (find 'island (aref overworld-map y (cadadr output-locs)))
		  do (push 'river-out (aref overworld-map y (cadadr output-locs))))
		   (push `(river-out ,(cadr output-locs) down) *special-biome-identifiers*)
		   (cadr output-locs))))
;Pushed to *special-biome-identifiers*: the big-lake/river-out source screen and the direction
; (up or down)
  

;Crashes if no options for input river are found; should first implement a
;change which resets and restarts generation; maybe should think of more elegant
;solution, which shifts map to handle lack of river options
(defun place-input-river (width height overworld-map input-locs)
  (labels ((unuseable (biome) 
					  (some 
						(lambda (x) (find x '(coast island small-lake big-lake
											  graveyard desert river-out)))
						biome))
		   (compatible (biome1 biome2)
				(let ((set1 '(mountain hills))
					  (set2 '(forest woods)))
				  (or (not biome1) (not biome2)
					  (and (find biome1 set1) (find biome2 set1))
					  (and (find biome1 set2) (find biome2 set2)))))
		   (path-helper (y x amt dir)
				 (if (and (> (funcall dir x) -1) (< (funcall dir x) width)
						  (not (unuseable (aref overworld-map y x)))
					   (compatible (car (aref overworld-map y x))
								 (car (aref overworld-map y (funcall dir x)))))
				   (path-helper y (funcall dir x) (1+ amt) dir)
				   amt))
		   (find-opts (y x end-dist dist dir acc)
				(if (> dist end-dist)
				  (remove-if (lambda (x) (> x 5)) (reverse acc))
				  (find-opts y (funcall dir x) end-dist (1+ dist) dir
							 (if (unuseable (aref overworld-map y x))
							   acc
							   (cons dist acc)))))
		   (pick-dist (dist-opts)
					  (nth (mrandom (length dist-opts)) dist-opts))
		   (path (start-y start-x)
				 (if (unuseable (aref overworld-map start-y start-x))
				   nil
				   (let* ((dist1 (path-helper start-y start-x 0 
											 (lambda (x) (1+ x))))
						  (opts1 (find-opts (1- start-y) (+ 1 start-x) dist1 2
											 (lambda (x) (1+ x)) nil))
						  (dist2 (path-helper start-y start-x 0 
											 (lambda (x) (1- x))))
						  (opts2 (find-opts (1- start-y) (- start-x 1) dist2 2
											 (lambda (x) (1- x)) nil)))
					 (cond ((and opts1 opts2)
								(if (zerop (mrandom 2))
								  `(right ,(pick-dist opts1))
								  `(left ,(pick-dist opts2))))
						    (opts1 `(right ,(pick-dist opts1)))
						    (opts2 `(left ,(pick-dist opts2)))
							(t nil)))))
		   (non-lake-neighbor (pos)
				(find-if 
				  (lambda (x) 
					(notany (lambda (y) (eq y 'big-lake))
							(aref overworld-map (car x) (cadr x))))
				  (find-neighbors width height pos)))
		   (handle-one-loc (loc)
				(let ((start (non-lake-neighbor loc)))
				  (when (and start (> (car start) 0))
					(append (path (car start) (cadr start))
							`(,start) `(,loc))))))
	(let* ((path-opts (remove-if (lambda (x) (not (cddr x)))
								 (mapcar #'handle-one-loc input-locs)))
		   (final-path (nth (mrandom (length path-opts)) path-opts))
		   (dir (if (eq (car final-path) 'left)
				  (lambda (start x) (- start x))
				  (lambda (start x) (+ start x))))
		   (origin (caddr final-path)))
	  (push 'river-in (aref overworld-map (car (cadddr final-path)) 
							(cadr (cadddr final-path))))
	  (loop for x below (cadr final-path)
			do (push 'river-in (aref overworld-map (car origin)
											(funcall dir (cadr origin) x))))
	  (push 'spring (aref overworld-map (1- (car origin)) 
									   (funcall dir (cadr origin) 
											(1- (cadr final-path)))))
	  (push `(river-in ,final-path) *special-biome-identifiers*))))
;Note: pushes river-in biome identifier. The identifier is of the form:
;([up-river direction (left or right)] [distance of river] 
; [last non-lake river-in position] [river mouth])

;Potential point of failure: no river entrance is usable. Need to write backup
(defun place-river (width height overworld-map)
  (let* ((lake-locs (collect-positions width height overworld-map 'big-lake))
		 (entry-pos (if (eql 2 (- (car (nth 6 lake-locs)) (caar lake-locs)))
						  '(1 3 5 10 12 14)
						  (if (eql (cadar lake-locs) (cadr (nth 3 lake-locs)))
							'(1 3 6 9 12 14)
							'(1 4 7 8 11 14))))
		 (river-entries (mapcar (lambda (x) (nth x lake-locs)) entry-pos))
		 (river-exit (place-output-river width height overworld-map
						(cons (first river-entries) (last river-entries)))))
	(place-input-river width height overworld-map 
					   (remove river-exit river-entries :test #'equal))))


(defun find-lone-empties (width height overworld-map)
  (loop for y below height append
		(loop for x below width
			  when (and (not (aref overworld-map y x))
						(every (lambda (x) 
								 (aref overworld-map (car x) (cadr x)))
							   (find-neighbors width height `(,y ,x))))
			  collect `(,y ,x))))



  
  

(defun fill-lone-empties (width height overworld-map lone-empties fillers)
  (labels ((fill-one (lone-empties fillers num-fillers)
			  (if (and lone-empties (> num-fillers 0))
				(let* ((rand-n (mrandom (length lone-empties)))
					   (rand-pos (nth rand-n lone-empties))
					   (rand-filler 
						 (if (eql (car rand-pos) (1- height))
						   (if (eq (caar fillers) 'open-ruin)
							 (pop-nth-in-freq-list 0 fillers)
							 nil)
						   (pop-nth-in-freq-list (mrandom num-fillers) fillers))))
				  (when rand-filler
					(push rand-filler
						(aref overworld-map (car rand-pos) (cadr rand-pos))))
				  (fill-one (feature-filter-coords width height overworld-map
										(append (subseq lone-empties 0 rand-n)
												(subseq lone-empties (1+ rand-n))))
							  fillers (if rand-filler (1- num-fillers) num-fillers)))
				(if lone-empties
				  nil
				  fillers))))
	(fill-one (feature-filter-coords width height overworld-map lone-empties)
			  fillers (freq-list-length fillers))))
						


(defun enumerate-dist (origin dist)
  (append
	`(,origin)
	(loop for y from 1 upto dist append
		  `((,(+ (car origin) y) ,(cadr origin))
			(,(- (car origin) y) ,(cadr origin))))
	(loop for x from 1 upto dist append
		  `((,(car origin) ,(+ (cadr origin) x))
			(,(car origin) ,(- (cadr origin) x))))
	(loop for y from 1 upto dist append
		  (loop for x from 1 upto (- dist y) append
			  `((,(+ (car origin) y) ,(+ (cadr origin) x))
			    (,(+ (car origin) y) ,(- (cadr origin) x))
			    (,(- (car origin) y) ,(+ (cadr origin) x))
			    (,(- (car origin) y) ,(- (cadr origin) x)))))))

(defun valid-coordp (width height coord)
  (and (> (car coord) -1) (< (car coord) height)
	   (> (cadr coord) -1) (< (cadr coord) width)))

(defun get-coords-list (width height)
  (loop for y below height append
		(loop for x below width collect
			  `(,y ,x))))

(defun place-feature (width height overworld-map feature min-dist valid-coords)
  (labels ((place-one (valid-coords locs cnt)
			  (if (zerop cnt)
				locs
				(let ((loc (nth (mrandom (length valid-coords)) valid-coords)))
				  (push (car feature) (aref overworld-map (car loc) (cadr loc)))
				  (place-one (remove-if (lambda (x) 
										  (member x (enumerate-dist loc min-dist)
												  :test #'equal))
										valid-coords)
							 (cons loc locs)
							 (1- cnt))))))
	(let ((coords-too-close 
			(mapcan (lambda (y) (enumerate-dist y min-dist))
					(remove-if (lambda (x) 
								 (not (member (car feature)
											  (aref overworld-map (car x) (cadr x)))))
							   (get-coords-list width height))))
		  (bad-bottom-coords (if (member (car feature) '(a-fairy pond))
							   (mapcar (lambda (x) (list (1- height) x))
									   (interval 0 (1- width)))
							   nil)))
	  (place-one (remove-if 
				   (lambda (x) (member x (append coords-too-close bad-bottom-coords)
									   :test #'equal))
				   valid-coords) nil (cadr feature)))))

(defun feature-filter-coords (width height overworld-map coords)
  (let* ((exclusive-biome-lst '(coast island big-lake small-lake river-in river-out
							    spring desert graveyard open-ruin pond a-fairy))
		 (bad-neighbors '(spring open-ruin pond a-fairy)))
	(remove-if (lambda (loc)
				 (or
				   (equal loc '(0 0))
				   (some #'identity 
					   (mapcar (lambda (biome) (member biome exclusive-biome-lst))
							   (aref overworld-map (car loc) (cadr loc))))
				   (some #'identity
						 (mapcar (lambda (x) 
							 (and (valid-coordp width height x) 
								  (intersection (aref overworld-map (car x) (cadr x))
												bad-neighbors)))
								 `((,(1- (car loc)) ,(cadr loc)) 
								   (,(1+ (car loc)) ,(cadr loc)))))
				   (and (> (car loc) 1) 
						(member 'island (aref overworld-map 
											  (- (car loc) 2) (cadr loc))))))
			   coords)))

(defun place-features (width height overworld-map features dists)
  (labels ((place-one (feat-lst coords)
			  (when feat-lst
				(let ((new-coords (place-feature width height overworld-map
												 (car feat-lst) 
												 (cadr (assoc (caar feat-lst)
															  dists)) coords)))
				  (place-one (cdr feat-lst)
							 (remove-if 
							   (lambda (x)
								 (member x 
									(append new-coords
											(mapcar (lambda (x) `(,(1- (car x))
															  ,(cadr x)))
											   new-coords)
											(mapcar (lambda (x) `(,(1+ (car x))
															  ,(cadr x)))
											   new-coords))
									:test #'equal))
							   coords))))))
	(place-one features (feature-filter-coords width height overworld-map 
									   (get-coords-list width height)))))

(defun find-bordering-biomes (width height overworld-map markers origin)
  (let ((already-searched nil)
		(border-list nil))
	(labels ((search-one (loc)
				(unless (member loc already-searched :test #'equal)
				  (push loc already-searched)
				  (if (aref overworld-map (car loc) (cadr loc))
					(let ((biome (find-if (lambda (x) 
											(member x '(forest woods
														hills mountain)))
										  (aref overworld-map (car loc)
												(cadr loc)))))
					  (when biome
						(push (list loc biome) border-list)))
					(mapc #'search-one (find-neighbors width height loc))))))
	  (search-one origin)
	  border-list)))

(defun dijkstra (width height chunks chunk-num sources)
  (let ((info (mapcar (lambda (x) (list (car x) (list 0 (cadr x)))) sources))
		(heap (build-min-heap (coerce (mapcar (lambda (x) (list 0 (car x)))
											  sources) 'array))))
	(labels ((get-dist (coord)
					   (caadr (assoc coord info :test #'equal)))
			 (get-biome (coord)
						(cadadr (assoc coord info :test #'equal)))
			 (handle-one (item)
				(let ((dist (car item)) (coord (cadr item)))
					(mapc (lambda (x)
							(if (find-if (lambda (y) (equal x (car y)))
										 info)
								(when (> (get-dist x) (1+ dist))
								  (push (list x (list (1+ dist)
												(get-biome coord)))
										info)
								  (decrease-key-by-data heap
								    (lambda (y) (equal (cadr y) x))
								    (1+ dist)))
								(when (eql chunk-num 
										   (aref chunks (car x) (cadr x)))
								  (push (list x (list (1+ dist)
													  (get-biome coord)))
										info)
								  (heap-insert heap (1+ dist) x))))
						  (find-neighbors width height coord))
					(if (zerop dist)
					  nil
					  `((,coord ,(get-biome coord))))))
			 (search-one (acc)
						 (if (< (heap-len heap) 1)
						   acc
						   (search-one 
							 (append (handle-one (extract-min heap)) acc)))))
	  (search-one nil))))

(defun fill-remaining-biomes (width height overworld-map markers)
  (let* ((already-found nil)
		 (starts (loop for y below height append
					   (loop for x below width 
							 when 
							 (and (aref markers y x) 
								  (not (find (aref markers y x) already-found)))
							 collect (progn 
									   (push (aref markers y x) already-found)
									  `(,(aref markers y x)
										 (,y ,x)))))))
	(mapc
	  (lambda (x) (push (cadr x) (aref overworld-map (caar x) (cadar x))))
	  (mapcan (lambda (x) 
				(let ((border (find-bordering-biomes width height overworld-map
													 markers (cadr x))))
				  (if border
					(dijkstra width height markers (car x) border)
					(let ((biome (nth (mrandom 4)
									  '(forest woods hills mountain))))
					  (loop for y below height do
							(loop for z below width 
								  when (eql (aref markers y z) (car x))
								  do (push biome (aref overworld-map y z))))))))
			starts))))

(defun clear-2d-array (width height arr2d)
  (loop for y below height do
		(loop for x below width do
			  (setf (aref arr2d y x) nil))))

(defun augment-water-biomes (width height overworld-map)
  (let* ((map-coords (get-coords-list width height))
		 (river-coords
		   (remove-if 
			 (lambda (x) 
			   (not (member 'river-in (aref overworld-map (car x) (cadr x)))))
			 map-coords))
		 (lake-coords
		   (remove-if
			 (lambda (x)
			   (not (or (equal '(big-lake) 
							   (remove 'river-out
									   (aref overworld-map (car x) (cadr x))))
						(equal '(small-lake)
							   (remove 'river-out 
									   (aref overworld-map 
											 (car x) (cadr x)))))))
			 map-coords))
		 (empty-out-river-coords
		   (remove-if 
			 (lambda (x) 
			   (not (equal '(river-out) (aref overworld-map (car x) (cadr x)))))
			 map-coords))
		 (augmenting-biomes '(forest woods hills mountains)))
	(labels ((get-river-info (coords biomes empty-coords)
				(if coords
				  (let ((extra-biome
						  (car (intersection (aref overworld-map (caar coords)
											  (cadar coords)) augmenting-biomes))))
					(if extra-biome
					  (if (member extra-biome biomes)
						(get-river-info (cdr coords) biomes empty-coords)
						(get-river-info (cdr coords) (cons extra-biome biomes)
										empty-coords))
					  (get-river-info (cdr coords) biomes (cons (car coords)
																empty-coords))))
				  (values biomes empty-coords)))
			 (fill-empty-river-coord (biomes empty-coord)
				(let ((biome-coord
						(find-if (lambda (x) 
								   (intersection augmenting-biomes
									(aref overworld-map (car x) (cadr x))))
								 (rand-bfs width height empty-coord
										   (lambda (x) 
											 (not 
											   (member x river-coords
													   :test #'equal)))))))
				  (push (car (intersection (aref overworld-map (car biome-coord)
												 (cadr biome-coord))
										   augmenting-biomes))
						(aref overworld-map (car empty-coord) 
											(cadr empty-coord)))))
			 (find-biome (start-coords)
				(let ((queue (make-queue))
					  (marks (make-array `(,height ,width))))
				  (labels ((handle-coord (coord) (push-queue coord queue)
												 (setf (aref marks (car coord)
															 (cadr coord)) t))
						   (bfs (coord)
								(let ((biome (intersection augmenting-biomes
												  (aref overworld-map (car coord)
														(cadr coord)))))
								  (if biome
									(car biome)
									(let ((neighbors
											(remove-if (lambda (x)
														 (aref marks 
															   (car x) (cadr x)))
													   (find-neighbors width
																	   height
																	   coord))))
									  (mapc #'handle-coord (shuffle neighbors))
									  (bfs (pop-queue queue)))))))
					(mapc #'handle-coord start-coords)
					(bfs (pop-queue queue)))))
			 (handle-lake-coord (coord)
								`(,coord ,(find-biome `(,coord)))))
	  (multiple-value-bind (biomes empties) (get-river-info river-coords nil nil)
		(when empties
		  (cond ((eql 1 (length biomes)) (mapc 
											(lambda (x) (push (first biomes)
															  (aref overworld-map
																	(car x)
																	(cadr x))))
											empties))
				((not biomes) (let ((biome (find-biome river-coords)))
								 (mapc (lambda (x) (push biome (aref overworld-map
																	 (car x)
																	 (cadr x))))
									   empties)))
				 (t (mapc (lambda (x) (fill-empty-river-coord biomes x))
						  empties)))))
	  (mapc (lambda (x) (push (cadr x) (aref overworld-map (caar x) (cadar x))))
			(mapcar #'handle-lake-coord (append empty-out-river-coords 
												lake-coords))))))


  
(defun place-biomes (width height overworld-map markers map-features)
  (princ 'placing-coast) (fresh-line)
  (place-coast width height 13 overworld-map)
  (princ 'big-lake) (fresh-line)
  (place-big-lake width height overworld-map *base-lake-offsets*)
  (princ 'grave) (fresh-line)
  (place-graveyard width height overworld-map *base-graveyard-offsets*)
  (princ 'desert) (fresh-line)
  (place-desert width height overworld-map)
  (princ 'small-lake) (fresh-line)
  (place-small-lake width height overworld-map)
  (princ 'chunks) (fresh-line)
  (find-chunks width height overworld-map markers)
  (princ 'mountain) (fresh-line)
  (place-mountains width height overworld-map markers)
  (princ 'forest) (fresh-line)
  (place-forest width height overworld-map 4 'forest)
  (princ 'woods) (fresh-line)
  (place-forest width height overworld-map 4 'woods)
  (princ 'clear-marks) (fresh-line)
  (clear-2d-array width height markers)
  (princ 'chunks2) (fresh-line)
  (find-chunks width height overworld-map markers)
  (princ 'place-hills) (fresh-line)
  (place-hills width height overworld-map markers)
  (princ 'expand-small) (fresh-line)
  (expand-small-biomes width height overworld-map *biome-size-alist*)
  (princ 'place-river) (fresh-line)
  (place-river width height overworld-map)
  (princ 'lone-empties) (fresh-line)
  (fill-lone-empties width height overworld-map 
					 (find-lone-empties width height overworld-map) map-features)
  (princ 'features) (fresh-line)
  (place-features width height overworld-map map-features *feature-distances*)
  (princ 'clear-marks2) (fresh-line)
  (clear-2d-array width height markers)
  (princ 'chunks3) (fresh-line)
  (find-chunks width height overworld-map markers)
  (princ 'fill-biomes) (fresh-line)
  (fill-remaining-biomes width height overworld-map markers)
  (display-biomes width height overworld-map)
  (augment-water-biomes width height overworld-map)
  (display-biomes width height overworld-map))


(defun make-biomes (width height)
  (let ((biomes (make-array (list height width)))
		(markers (make-array (list height width)))
		(map-features (deep-copy-list *map-features*)))
	(place-biomes width height biomes markers map-features)
	biomes))

(defun get-screen-biome (overworld-map y x)
  (if (valid-screen overworld-map y x)
	(aref overworld-map y x)
	'(none)))

(defun reduce-biome (biome)
  (cadr
	(reduce (lambda (x y) (if (< (car x) (car y)) x y))
			(mapcar (lambda (x) `(,(position x +biome-priority+) ,x))
					biome))))


(defun display-biomes (width height overworld-map)
  (format t "~{~<~%~,32:;~1a ~>~}"
		  (loop for y below height
				append (loop for x below width
							 collect (if (aref overworld-map y x)
									   (substring (symbol-name 
										 (car (aref overworld-map y x)))
										0 1)
									   "_")))))

(defun display-marks (width height marks)
  (format t "~{~<~%~,32:;~1a ~>~}"
		  (loop for y below height
				append (loop for x below width
							 collect (if (aref marks y x)
										 (aref marks y x)
										 "N")))))



