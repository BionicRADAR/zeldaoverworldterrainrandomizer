;;This file is part of a program to construct new overworlds in
;;The Legend of Zelda. It handles edges between screens, figuring out
;;where the map should connect and constructing a datastructure of edges.
;;Author: Nathaniel Schleicher

;List of edge types:
;none: edge is not on map
;edge: connected by normally passable terrain (ladder or open)
;wall: blocked by normal blocking terrain (rock or tree)
;water-wall: blocked by water (used in big lake or coast)
;dock: connected by raft dock
;bridge: connected by a bridge

(defparameter *horizontal-edges* nil)
(defparameter *vertical-edges* nil)
(defparameter *lake-wall* nil)
(defparameter *secret-passage* nil)
(defparameter *connect-graph* nil)
(defparameter *connect-graph-array* nil)
;Ratios go in order '(horiz vert)
;NOTE: FIGURE OUT RIVER-IN; I think done already
(defconstant +all-edges-ratios+ '(90/120 70/112))
(defconstant +mountain-internal-ratios+ '(20/22 9/13))
(defconstant +mountain-external-ratios+ '(9/13 3/12))
(defconstant +forest-internal-ratios+ '(7/7 6/6))
(defconstant +forest-external-ratios+ '(13/17 5/14))
(defconstant +woods-internal-ratios+ '(6/6 5/5))
(defconstant +woods-external-ratios+ '(3/4 2/4))
(defconstant +hills-internal-ratios+ '(5/5 3/3))
(defconstant +hills-external-ratios+ '(5/7 2/7))
(defconstant +coast-edge-ratio+ 3/8)
(defconstant +coast-cluster-ratio+ 4/6)
(defconstant +desert-ratio+ 4/7)
(defconstant +grave-ratio+ 2/5)
(defconstant +big-lake-ratio+ 7/18)
(defconstant +small-lake-ratio+ 5/8)
(defconstant +internal-ratio-hash+ (make-hash-table))
(defconstant +external-ratio-hash+ (make-hash-table))

(mapc (lambda (x) (setf (gethash (car x) +internal-ratio-hash+) (cdr x)))
	  `((mountain . ,+mountain-internal-ratios+)
		(forest . ,+forest-internal-ratios+)
		(woods . ,+woods-internal-ratios+)
		(hills . ,+hills-internal-ratios+)))
(mapc (lambda (x) (setf (gethash (car x) +external-ratio-hash+) (cdr x)))
		`((mountain . ,+mountain-external-ratios+)
		  (forest . ,+forest-external-ratios+)
		  (woods . ,+woods-external-ratios+)
		  (hills . ,+hills-external-ratios+)
		  (coast-edge . ,+coast-edge-ratio+)
		  (coast-cluster . ,+coast-cluster-ratio+)
		  (desert . ,+desert-ratio+)
		  (graveyard . ,+grave-ratio+)
		  (big-lake . ,+big-lake-ratio+)
		  (small-lake . ,+small-lake-ratio+)))

(defun get-internal-ratio (biome)
  (gethash biome +internal-ratio-hash+))

(defun get-external-ratio (biome)
  (gethash biome +external-ratio-hash+))

(defun get-biome-ratio (biome internal-p)
  (if (eq int-or-ext 'internal)
	(get-internal-ratio biome)
	(get-external-ratio biome)))

(defparameter *need-east-exit-ruin* nil)
(defconstant +dirs+ '(north south east west))

(define-condition no-valid-edges-error (error)
  ((screen-coords :initarg :screen-coords :reader screen-coords)))

(defun north-ladder-p (biome)
  (and biome (not (intersection biome '(a-fairy pond open-ruin river-in
										island forest woods desert none)))))

(defun side-exit-p (biome)
  (and biome (not (intersection biome '(a-fairy pond open-ruin island none)))))

(defun init-edges (width height)
  (setf *horizontal-edges* (make-array (list height (1- width))))
  (setf *vertical-edges* (make-array (list (1- height) width)))
  (setf *lake-wall* nil)
  (setf *secret-passage* nil)
  (setf *connect-graph* nil)
  (setf *connect-graph-array* nil))

(defun edge-on-map-p (edge-array y x)
  (and (< y (array-dimension edge-array 0)) (> y -1)
	   (< x (array-dimension edge-array 1)) (> x -1)))

(defun get-edge (edge-array y x)
  (if (edge-on-map-p edge-array y x)
	(aref edge-array y x)
	'none))

(defun passable-edge-p (edge-array y x)
  (member (get-edge edge-array y x) '(edge dock bridge)))

(defun set-edge (edge-array y x new-edge)
  (when (edge-on-map-p edge-array y x)
	(setf (aref edge-array y x) new-edge)))


(defun biome-dir (overworld-map y x dir)
  (let ((diff (assoc dir '((north -1 0) (south 1 0) (east 0 1) (west 0 -1)))))
	(get-screen-biome overworld-map 
					  (+ y (cadr diff)) (+ x (caddr diff)))))

(defun north-edge (horizontals verticals y x)
  (get-edge verticals (1- y) x))

(defun south-edge (horizontals verticals y x)
  (get-edge verticals y x))

(defun west-edge (horizontals verticals y x)
  (get-edge horizontals y (1- x)))

(defun east-edge (horizontals verticals y x)
  (get-edge horizontals y x))

;Returns edges of coordinate in order north south west east
(defun collect-edges (horizontals verticals y x)
  (mapcar (lambda (dir) (funcall dir horizontals verticals y x))
		  (list #'north-edge #'south-edge #'west-edge #'east-edge)))

(defun add-fairy-edges (horizontals verticals overworld-map coords)
  (mapc (lambda (x) 
		  (set-edge verticals (car x) (cadr x) 
					(if (and (not *need-east-exit-ruin*)
						  (equal (aref overworld-map (car x) (cadr x))
								 'mountain))
					  'ladder
					  'edge))
		  (set-edge verticals (1- (car x)) (cadr x) 'wall)
		  (set-edge horizontals (car x) (1- (cadr x)) 'wall)
		  (set-edge horizontals (car x) (cadr x) 'wall))
		coords))

(defun add-pond-edges (horizontals verticals overworld-map coords)
  (add-fairy-edges horizontals verticals overworld-map coords))

(defun add-island-edges (horizontals verticals overworld-map coords)
  (mapc (lambda (x) 
		  (set-edge verticals (1- (car x)) (cadr x) 'water-wall)
		  (set-edge horizontals (car x) (1- (cadr x)) 'water-wall)
		  (set-edge horizontals (car x) (cadr x) 'water-wall)
		  (set-edge verticals (car x) (cadr x) 'dock))
		coords)
  (let* ((lake-islands (remove-if (lambda (x) (member 'coast 
													 (aref overworld-map
														   (car x)
														   (cadr x))))
								 coords))
		 (bad-island (find-if (lambda (x) (eql (car x) 
											   (- (array-dimension
												    overworld-map 0) 2)))
					 lake-islands))
		 (bridge-island (if bad-island bad-island
						  (nth (mrandom 2) lake-islands)))
		 (unavailable (mapcar (lambda (x)
								(member 'big-lake 
										(get-screen-biome overworld-map
												(car bridge-island)
												(+ (cadr bridge-island) x))))
							  '(-2 2))))
	(set-edge horizontals (car bridge-island)
			  (+ (cadr bridge-island)
				 (cond ((car unavailable) 0)
					   ((cadr unavailable) -1)
					   (t (1- (mrandom 2)))))
			  'bridge)
	(set-edge verticals (car bridge-island) (cadr bridge-island) 'water-wall)))

(defun get-edge-neighbors-that (horizontals verticals overworld-map coord condp)
	(mapcan (lambda (x edge-array edge-diff)
			  (let ((neighbor-y (+ (car coord) (car x)))
					(neighbor-x (+ (cadr coord) (cadr x))))
				(if (and (valid-screen overworld-map neighbor-y neighbor-x)
						 (funcall condp (get-edge edge-array 
										(+ (car coord) (car edge-diff))
										(+ (cadr coord) (cadr edge-diff)))))
				  `((,(lambda (y) (set-edge edge-array 
											(+ (car coord) (car edge-diff))
											(+ (cadr coord) (cadr edge-diff))
											y))
					  (,neighbor-y ,neighbor-x) (,(car coord) ,(cadr coord))))
				  nil)))
			'((-1 0) (1 0) (0 -1) (0 1))
			`(,verticals ,verticals ,horizontals ,horizontals)
			'((-1 0) (0 0) (0 -1) (0 0))))

(defun get-all-edge-neighbors (horizontals verticals overworld-map coord)
  (get-edge-neighbors-that horizontals verticals overworld-map coord
						   (lambda (x) t)))

;a function that finds edges of a given coord that have not yet been specified
;returns a list of lists that contain edge info, each element in this order:
;	(setter other-screen-coords input-screen-coords)
(defun get-edge-neighbors (horizontals verticals overworld-map coord)
  (get-edge-neighbors-that horizontals verticals overworld-map coord #'not))

(defun add-ruin-edges (horizontals verticals overworld-map coords)
  (mapc (lambda (x)
		  (set-edge verticals (1- (car x)) (cadr x) 'wall)
		  (cond ((north-ladder-p (biome-dir overworld-map (car x) (cadr x) 
											'south))
					(set-edge verticals (car x) (cadr x) 'edge)
					(set-edge horizontals (car x) (1- (cadr x)) 'wall)
					(set-edge horizontals (car x) (cadr x) 'wall))
				((side-exit-p (biome-dir overworld-map (car x) (cadr x) 
											'west))
					(set-edge verticals (car x) (cadr x) 'wall)
					(set-edge horizontals (car x) (1- (cadr x)) 'edge)
					(set-edge horizontals (car x) (cadr x) 'wall))
				((side-exit-p (biome-dir overworld-map (car x) (cadr x) 'east))
					(set-edge verticals (car x) (cadr x) 'wall)
					(set-edge horizontals (car x) (1- (cadr x)) 'wall)
					(set-edge horizontals (car x) (cadr x) 'edge)
					(setf *need-east-exit-ruin* t))
				((side-exit-p (biome-dir overworld-map (car x) (cadr x)
										 'south))
					(set-edge verticals (car x) (cadr x) 'edge)
					(set-edge horizontals (car x) (1- (cadr x)) 'wall)
					(set-edge horizontals (car x) (cadr x) 'wall))
				(t (error 'no-valid-edges-error :screen-coords x))))
		coords))

(defun add-spring-edges (horizontals verticals overworld-map coords)
  (mapc (lambda (x)
		  (set-edge verticals (car x) (cadr x) 'edge)
		  (set-edge verticals (1- (car x)) (cadr x) 'wall)
		  (unless (get-edge horizontals (car x) (1- (cadr x)))
			(set-edge horizontals (car x) (1- (cadr x)) 'wall))
		  (unless (get-edge horizontals (car x) (cadr x))
			(set-edge horizontals (car x) (cadr x) 'wall)))
		coords))

;externals are expected to be in the form given by get-edge-neighbors, namely
;	(setter out-of-biome-coord given-coord)
(defun filter-externals-by-biome (externals overworld-map)
  (let ((types nil)
		(filtered-edges nil))
	(mapc (lambda (x) 
			(let ((biome (reduce-biome (get-screen-biome overworld-map 
													(caadr x) (cadadr x)))))
			  (unless (member biome types)
				(push biome types)
				(push (list biome) filtered-edges))
			  (push x (cdr (assoc biome filtered-edges)))))
		  externals)
	filtered-edges))

(defun handle-single-edge-set (edges edge-ratio)
  (mapc (lambda (x) (funcall (car (pop-nth edges (mrandom (length edges)))) 
							 'edge))
		(interval 1 (if (> (round (* edge-ratio (length edges))) 1)
					  (round (* edge-ratio (length edges))) 1)))
  (mapc (lambda (x) (funcall (car x) 'wall)) edges))

(defun handle-multi-edge-set (edges horiz-ratio vert-ratio)
  (let ((hweight (* (numerator horiz-ratio) (denominator vert-ratio)))
		(vweight (* (numerator vert-ratio) (denominator horiz-ratio)))
		(h-edges (remove-if (lambda (x) (not (horiz-edge-p (cadr x) (caddr x))))
							edges))
		(v-edges (remove-if (lambda (x) (horiz-edge-p (cadr x) (caddr x)))
							edges)))
	(mapc (lambda (x)
			(if (< (mrandom (+ (* hweight (length h-edges)) 
							   (* vweight (length v-edges))))
				   (* hweight (length h-edges)))
			  (funcall (car (pop-nth h-edges (mrandom (length h-edges))))
					   'edge)
			  (funcall (car (pop-nth v-edges (mrandom (length v-edges))))
					   'edge)))
		  (interval 1 (let ((num (round (+ (* horiz-ratio (length h-edges))
										   (* vert-ratio (length v-edges))))))
						(if (> num 1) num 1))))
	(mapc (lambda (x) (funcall (car x) 'wall)) (append h-edges v-edges))))
  

(defun handle-edge-set (edges ratio1 ratio2)
  (if (or (listp ratio1) (listp ratio2))
	(handle-multi-edge-set edges (/ (+ (if (listp ratio1) (car ratio1) ratio1)
									(if (listp ratio2) (car ratio2) ratio2)) 2)
							(/ (+ (if (listp ratio1) (cadr ratio1) ratio1)
								  (if (listp ratio2) (cadr ratio2) ratio2)) 2))
	(handle-single-edge-set edges (/ (+ ratio1 ratio2) 2))))

(defun grave-desert-edge-helper (horizontals verticals overworld-map coords rat)
  (let ((externals (remove-if #'not (mapcan (lambda (x)
			(mapcar (lambda (y)
					  (if (internal-edge-p x (cadr y) overworld-map)
						(progn (funcall (car y) 'edge) nil)
						(progn (funcall (car y) 'pending) y)))
					(get-edge-neighbors horizontals verticals  
										overworld-map x)))
				   coords))))
	(mapc (lambda (x) (handle-edge-set (cdr x) rat
									   (get-external-ratio (car x))))
		  (filter-externals-by-biome externals overworld-map))))

(defun add-grave-edges (horizontals verticals overworld-map coords)
  (grave-desert-edge-helper horizontals verticals 
							overworld-map coords +grave-ratio+))

(defun add-desert-edges (horizontals verticals overworld-map coords)
  (grave-desert-edge-helper horizontals verticals 
							overworld-map coords +desert-ratio+))

(defun add-river-edges (horizontals verticals overworld-map coords)
  (mapc (lambda (x)
		  (mapc (lambda (y)
				  (if (or (member 'river-in 
								  (get-screen-biome overworld-map 
													(caadr y) (cadadr y)))
						  (horiz-edge-p x (cadr y)))
					(funcall (car y) 'edge)
					(funcall (car y) 'wall)))
				(get-edge-neighbors horizontals verticals overworld-map x)))
		(remove-if (lambda (x) (member 'big-lake 
									   (aref overworld-map (car x) (cadr x)))) 
				   coords)))

(defun lake-edge-helper (horizontals verticals overworld-map coords biome rat)
  (let ((externals (remove-if #'not (mapcan (lambda (x)
			(mapcar (lambda (y)
					  (if (member biome (get-screen-biome overworld-map 
														  (caadr y) (cadadr y)))
						(progn (funcall (car y) 'edge) nil)
						(progn (funcall (car y) 'pending) y)))
					(get-edge-neighbors horizontals verticals  
										overworld-map x)))
				   coords))))
	(mapc (lambda (x) (handle-edge-set (cdr x) rat
									   (get-external-ratio (car x))))
		  (filter-externals-by-biome externals overworld-map))))


(defun add-big-lake-edges (horizontals verticals overworld-map coords)
  (let* ((biome-ids (cdr (assoc 'big-lake *special-biome-identifiers*)))
		(dock-coord (find-if (lambda (x) (eq (north-edge horizontals verticals
													 (car x) (cadr x)) 'dock))
							 coords))
		(origin (car biome-ids)))
	(if (< (cadr biome-ids) 2)
	  (set-edge verticals (+ (car origin) 1) (+ (cadr origin) 2) 'water-wall)
	  (set-edge horizontals (+ (car origin) 2) (+ (cadr origin) 1) 'water-wall))
	(set-edge verticals (car dock-coord) (cadr dock-coord) 
			  (if (intersection '(spring open-ruin a-fairy island pond)
								(aref overworld-map (1+ (car dock-coord))
									  (cadr dock-coord)))
				'wall 'edge))
  (lake-edge-helper horizontals verticals overworld-map coords 'big-lake
					+big-lake-ratio+)
  (let ((wall-screen (find-if (lambda (x) (and (member 'river-in 
										  (aref overworld-map (car x) (cadr x)))
								  (some (lambda (y)
										(member 'river-in
												(aref overworld-map
													  (car x) (+ (cadr x) y))))
									  '(-1 1))))
							  coords)))
	(if wall-screen ;Wall-screen is the big-lake river-in coord if ;river-mouth is horizontal; otherwise it's nil
	  (if (equal dock-coord `(,(1- (car wall-screen)) ,(cadr wall-screen)))
		(set-edge verticals (car wall-screen) (cadr wall-screen) 'wall)
		(set-edge verticals (1- (car wall-screen)) (cadr wall-screen) 'wall))
	  (let ((wall-opts
		  (remove-if (lambda (x) (or (not (member 'big-lake 
									  (get-screen-biome overworld-map
														(caadr x) (cadadr x))))
									 (intersection '(island river-in river-out)
									  (append (get-screen-biome overworld-map
														(caadr x) (cadadr x))
											  (get-screen-biome overworld-map
														(caaddr x)
														(cadadr (cdr x)))))
									 (if (> (1- (caaddr x)) -1)
									   (eq (get-edge verticals (1- (caaddr x))
												 (cadadr (cdr x)))
										   'dock)
									   nil)))
			 (apply #'append 
					(mapcar (lambda (x) 
							  (get-all-edge-neighbors horizontals verticals 
													  overworld-map x))
							coords)))))
	(let ((lake-wall (nth (mrandom (length wall-opts)) wall-opts)))
	  (funcall (car lake-wall) 'wall)
	  (setf *lake-wall* (cdr lake-wall))))))))

(defun add-small-lake-edges (horizontals verticals overworld-map coords)
  (lake-edge-helper horizontals verticals overworld-map coords 'small-lake
					+small-lake-ratio+))

(defun add-spec-coast-cluster-edges (horizontals verticals overworld-map)
  (let* ((biome-ids (cdr (assoc 'coast-cluster *special-biome-identifiers*)))
		 (loc (car biome-ids))
		 (dir (cadr biome-ids))
		 (vert (caddr biome-ids))
		 (horiz (cadddr biome-ids)))
	(cond ((eq dir 'vertical) (set-edge verticals 
										(funcall vert 
												 (if (member loc
															 '(bottom-right
															   bottom-left))
												   2 1))
										(funcall horiz 1) 'water-wall))
		  ((eq dir 'horizontal) (set-edge horizontals 
										  (funcall vert 1) 
										  (funcall 
											horiz 
											(cond
											  ((eq loc 'top-left) 1)
											  ((eq loc 'top-right) 2)
											  ((eq loc 'bottom-left) 2)
											  ((eq loc 'bottom-right) 3)))
										  'water-wall)
								(when (member loc '(bottom-right bottom-left))
								  (set-edge horizontals 
											(funcall vert 0) 
											(funcall horiz 
													 (if (eq loc 'bottom-right)
													   3 2)) 'bridge)))
		  ((not dir) (set-edge 
				 horizontals
				 (funcall vert 1)
				 (funcall horiz 
						  (if (member 'coast (aref overworld-map
														  (funcall vert 2)
														  (funcall horiz 0)))
							(if (eq loc 'top)
							  0
							  (progn (set-edge horizontals
											   (funcall vert 0)
											   (funcall horiz 2) 'bridge)
									 2))
							(progn (when (eq loc 'bottom)
									 (set-edge horizontals
											   (funcall vert 0)
											   (funcall horiz 1) 'bridge))
								   1))) 'water-wall)))))

(defun add-coast-cluster-edges (horizontals verticals overworld-map coords)
  (add-spec-coast-cluster-edges horizontals verticals overworld-map)
  (lake-edge-helper horizontals verticals overworld-map coords 'coast 
					+coast-cluster-ratio+))

(defun add-coast-edge-edges (horizontals verticals overworld-map coords)
  (lake-edge-helper horizontals verticals overworld-map coords 'coast
					+coast-edge-ratio+))


(defconstant +special-edge-functions+
			 (list #'add-ruin-edges #'add-fairy-edges #'add-pond-edges 
			       #'add-spring-edges #'add-island-edges #'add-river-edges
			       #'add-coast-cluster-edges #'add-coast-edge-edges 
			       #'add-grave-edges #'add-desert-edges  
			       #'add-big-lake-edges #'add-small-lake-edges))

(defun internal-edge-p (coord0 coord1 overworld-map)
  (equal (remove 'river-out (aref overworld-map (car coord0) (cadr coord0)))
		 (remove 'river-out (aref overworld-map (car coord1) (cadr coord1)))))

(defun horiz-edge-p (coord0 coord1)
  (eql (car coord0) (car coord1)))

(defun get-feature-coords (width height overworld-map)
  (let* ((feature-order '(open-ruin a-fairy pond spring island river-in))
		 (feature-coords (make-array (length feature-order))))
	(mapcar (lambda (y)
			  (mapcar (lambda (x)
						(let ((feature 
								(car (intersection feature-order 
												   (aref overworld-map y x)))))
						  (when feature
							(push `(,y ,x) 
								  (aref feature-coords
										(position feature feature-order))))))
					  (interval 0 15)))
			(interval 0 7))
	(coerce feature-coords 'list)))

(defun order-special-coords (spec-coords-list)
  (let ((spec-order '(coast-cluster coast-edge grave desert big-lake
								 small-lake)))
	(mapcar (lambda (biome) (cadr (assoc biome spec-coords-list)))
			spec-order)))

(defun get-special-coords (width height overworld-map spec-coords-list)
  (append (get-feature-coords width height overworld-map)
		  (order-special-coords spec-coords-list)))

(defun fill-special-edges (horizontals verticals overworld-map coords-lists)
  (mapcar (lambda (fun coords) 
			(funcall fun horizontals verticals overworld-map coords))
		  +special-edge-functions+ coords-lists))

(defun get-and-fill-spec-edges (horizontals verticals 
									  overworld-map spec-coords-list)
  (fill-special-edges horizontals verticals overworld-map 
			(get-special-coords (array-dimension overworld-map 1) 
								(array-dimension overworld-map 0)
								overworld-map spec-coords-list)))

(defun normal-edge-helper (horizontals verticals overworld-map coords 
									   internal-ratio external-ratio)
  (let ((externals nil)
		(internals nil))
	(mapc (lambda (coord)
			(mapc (lambda (neighbor)
					(funcall (car neighbor) 'pending)
					(if (member (cadr neighbor) coords :test #'equal)
					  (push neighbor internals)
					  (push neighbor externals)))
				(get-edge-neighbors horizontals verticals overworld-map coord)))
		  coords)
	(when internals
	  (handle-edge-set internals internal-ratio internal-ratio))
	(mapc (lambda (edge-set) (handle-edge-set (cdr edge-set) external-ratio 
											  (get-external-ratio 
												(car edge-set))))
		  (filter-externals-by-biome externals overworld-map))))

(defun fill-normal-edges (horizontals verticals overworld-map)
  (mapc (lambda (coord-set)
		  (normal-edge-helper horizontals verticals overworld-map 
							  (cadr coord-set)
							  (get-internal-ratio (caar coord-set))
							  (get-external-ratio (caar coord-set))))
		(list-biomes (array-dimension overworld-map 1) 
					 (array-dimension overworld-map 0)
					 overworld-map)))

(defun fill-edges (horizontals verticals overworld-map spec-coords-list)
  (get-and-fill-spec-edges horizontals verticals overworld-map spec-coords-list)
  (fill-normal-edges horizontals verticals overworld-map)
  (edge-connect-map (array-dimension overworld-map 1)
					(array-dimension overworld-map 0)
					horizontals verticals overworld-map))

(defun run-edge-adder (overworld-map spec-coords-list)
  (init-edges (array-dimension overworld-map 1) 
			  (array-dimension overworld-map 0))
  (fill-edges *horizontal-edges* *vertical-edges* overworld-map 
			  spec-coords-list))

(defun format-edges-and-biomes (horizontals verticals overworld-map)
  (let ((width (array-dimension overworld-map 1))
		(height (array-dimension overworld-map 0)))
	(format t "~{~<~%~,32:;~1a~>~}"
			(remove-if #'not
	(mapcan (lambda (y)
	(append
	  (mapcan (lambda (x) (list
						  (substring (symbol-name (reduce-biome 
												  (aref overworld-map y x)))
									 0 1)
						  (if (< x (1- width))
							(substring (symbol-name (aref horizontals y x))
									   0 1)
							" ")))
			  (interval 0 (1- width)))
	  (if (< y (1- height))
	  (mapcan (lambda (x) (list (substring (symbol-name (aref verticals y x))
										   0 1)
							    " "))
			  (interval 0 (1- width)))
	  nil)))
			(interval 0 (1- height)))))))

(defstruct connect-vertex data edges mark)
(defstruct connect-edge source dest kind)

(defun print-connect-edge (edge)
  (princ "#S(CONNECT-EDGE :SOURCE ")
  (princ (connect-vertex-data (connect-edge-source edge)))
  (princ " :DEST ")
  (princ (connect-vertex-data (connect-edge-dest edge)))
  (princ " :KIND ")
  (princ (connect-edge-kind edge))
  (princ ")"))

(defun print-connect-vertex (vertex)
  (princ "#S(CONNECT-VERTEX :DATA ")
  (princ (connect-vertex-data vertex))
  (princ " :EDGES (")
  (mapc (lambda (e) (print-connect-edge e) (princ " ")) 
		(connect-vertex-edges vertex))
  (princ ")")
  (princ " :MARK ")
  (princ (connect-vertex-mark vertex))
  (princ ")"))

(defun connect-dfs (vertex condit action marker get-dests)
  (unless (or (funcall condit vertex)
			  (connect-vertex-mark vertex))
	(setf (connect-vertex-mark vertex) marker)
	(funcall action vertex)
	(mapc (lambda (dest)
			(connect-dfs dest condit action marker get-dests))
		  (funcall get-dests vertex))))

(defun make-connect-edge-graph (width height horizontal-edges vertical-edges)
  (let ((vertex-array (make-array (list height width)))
		(coords (get-coords-list width height)))
	(mapc (lambda (coord)
			(setf (aref vertex-array (car coord) (cadr coord))
				  (make-connect-vertex :data coord)))
		  coords)
	(values
	  (mapcar (lambda (coord)
				(setf (connect-vertex-edges 
						(aref vertex-array (car coord) (cadr coord)))
					  (remove-if
						(lambda (edge) (member (connect-edge-kind edge) 
											   '(wall water-wall none)))
						(mapcar (lambda (dest kind)
								  (make-connect-edge
									:source coord
									:dest dest
									:kind kind))
								(mapcar (lambda (delta)
										  `(,(+ (car delta) (car coord))
										    ,(+ (cadr delta) (cadr coord))))
										'((-1 0) (1 0) (0 -1) (0 1)))
								(collect-edges horizontal-edges vertical-edges
												(car coord) (cadr coord)))))
			  (aref vertex-array (car coord) (cadr coord)))
		  coords)
	  vertex-array)))

(defun separate-to-connected-sets (graph)
  (let ((mark-to-pos (copy-seq '((0 . 0))))
		(connected-sets (copy-seq '(nil))))
	(mapc (lambda (vertex)
			(let ((pos (cdr (assoc (connect-vertex-mark vertex) mark-to-pos))))
			  (if pos
				(push vertex (nth pos connected-sets))
				(progn
				  (push `(,(connect-vertex-mark vertex) .
						  ,(length connected-sets))
						mark-to-pos)
				  (setf (cdr (last connected-sets)) `((,vertex)))))))
		  graph)
	connected-sets))

(defun get-arr2d (arr2d coord)
  (aref arr2d (car coord) (cadr coord)))

(defun set-arr2d (arr2d coord new-val)
  (setf (aref arr2d (car coord) (cadr coord)) new-val))

(defsetf get-arr2d set-arr2d)

(defun get-edge-between (horizontal-edges vertical-edges coord1 coord2)
  (aref
	(if (not (eql (car coord1) (car coord2)))
	  vertical-edges horizontal-edges)
	(min (car coord1) (car coord2))
	(min (cadr coord1) (cadr coord2))))


(defun edge-connect-map (width height horizontal-edges vertical-edges biomes)
  (multiple-value-bind (graph vertex-array) 
	(make-connect-edge-graph width height horizontal-edges vertical-edges)
	(mapc (lambda (vertex marker)
			(connect-dfs vertex (lambda (x) nil) (lambda (x) nil) marker
						 (lambda (vertex)
						   (mapcar (lambda (edge) 
									 (get-arr2d vertex-array 
												(connect-edge-dest edge)))
								   (connect-vertex-edges vertex)))))
		  graph
		  (interval 0 (1- (length graph))))
	(labels ((find-connections (vertices)
				(apply #'append
					   (mapcar 
						 (lambda (vertex)
						   (remove-if
							 (lambda (neighbor)
							   (let ((source (caddr neighbor))
									 (dest (cadr neighbor))
									 (river (cadr (assoc 'river-in
												*special-biome-identifiers*))))
								 (or (eql (connect-vertex-mark 
											(get-arr2d vertex-array dest)) 0)
									 (and (equal (cadddr river)
												 (if (< (car source) (car dest))
												   dest source))
										  (eql (car (cadddr river))
											   (caaddr river))))))
							 (get-edge-neighbors-that
							   horizontal-edges vertical-edges biomes
							   (connect-vertex-data vertex)
							   (lambda (x)
								 (member x '(wall))))))
						 vertices)))
			 (filter-biomes (edge-neighbors)
				(remove-if (lambda (edge-neighbor)
							 (some
							   (lambda (coord)
								 (intersection 
								   '(spring pond open-ruin a-fairy island)
								   (get-arr2d biomes coord)))
							   `(,(cadr edge-neighbor) ,(caddr edge-neighbor))))
						   edge-neighbors))
			 (make-secret-passage (edge-neighbors)
								  (mrandom 0) ;Temporary solution until I
											  ;properly implement secret
											  ;passages
				(let* ((filtered
						(remove-if (lambda (x)
									 (or
									   (some (lambda (coord)
											   (intersection '(island)
													(get-arr2d biomes coord)))
											 (cdr x))
									   (every (lambda (coord)
												(intersection
												  '(spring pond open-ruin
														   a-fairy)
												  (get-arr2d biomes coord)))
											  (cdr x))))
								   edge-neighbors))
					   (pick (nth (mrandom (length filtered)) filtered)))
				  (setf *secret-passage* (cdr pick))
				  pick))
			 (connect-unconnected (main-set other-sets)
				(when other-sets
				  (let* ((connections (find-connections main-set))
						 (biome-filtered (filter-biomes connections))
						 (pick
						   (if biome-filtered
							 (nth (mrandom (length biome-filtered)) 
								  biome-filtered)
							 (make-secret-passage connections)))
						 (dest-pos
						   (position-if
							 (lambda (s)
							   (some (lambda (v)
									   (equal (connect-vertex-data v)
											  (cadr pick)))
									 s))
							 other-sets)))
					(when biome-filtered
					  (funcall (car pick) 'edge))
					(mapc (lambda (source dest)
							(push (make-connect-edge 
									:source source
									:dest dest
									:kind (if biome-filtered 'edge 'secret))
								  (connect-vertex-edges
									(get-arr2d vertex-array source))))
						  `(,(caddr pick) ,(cadr pick))
						  `(,(cadr pick) ,(caddr pick)))
					(let ((old-mark (connect-vertex-mark 
									  (get-arr2d vertex-array (cadr pick)))))
					  (mapc (lambda (vertex)
							  (when (eql (connect-vertex-mark vertex) old-mark)
								(setf (connect-vertex-mark vertex) 0)))
							graph))
					(connect-unconnected 
					  (append main-set (nth dest-pos other-sets))
					  (append (subseq other-sets 0 dest-pos)
							  (nthcdr (1+ dest-pos) other-sets)))))))
	(let ((connected-sets (separate-to-connected-sets graph)))
	  (connect-unconnected (car connected-sets) (cdr connected-sets))))
	(setf *connect-graph* graph)
	(setf *connect-graph-array* vertex-array)))
