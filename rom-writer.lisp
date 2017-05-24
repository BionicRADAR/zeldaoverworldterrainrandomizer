(defparameter *final-ladder-screens* nil)
(defparameter *final-whistle-screens* nil)

(defun init-writer ()
  (setf *final-ladder-screens* nil)
  (setf *final-whistle-screens* nil))

(defun write-rom (filename screens base biomes vertex-array columns palettes
						   horiz-edges vert-edges spec-biome-info 
						   secret-passage)
  (let ((rom-array (copy-seq *byte-array*)))
	(write-columns rom-array columns)
	(choose-stepladder-screens rom-array vertex-array screens base biomes
							   horiz-edges vert-edges)
	(write-lost-screens rom-array)
	(write-palettes rom-array screens base biomes palettes)
	(write-indoor-palette-loc rom-array biomes)
	(write-stair-and-exit-info rom-array screens base)
	(write-whistle-info rom-array base biomes)
	(write-raft-info rom-array screens horiz-edges vert-edges)
	(write-secret-passage rom-array secret-passage) ;placeholder until secret
													;passage implemented
	(write-screens rom-array screens)
	(write-rom-to-file filename rom-array)))

(defun write-rom-to-file (filename rom-array)
  (with-open-file (out filename :direction :output :if-exists :supersede
					   :if-does-not-exist :create :element-type 
					   '(unsigned-byte 8))
	(mapc (lambda (rom-byte)
			(write-byte rom-byte out))
		  (coerce rom-array 'list))))


(defun write-screens (rom-array screens)
  (let ((out-screens (make-array 124))
		(screen-refs (make-array 128))
		(duplicate-nums nil)
		(current-index 0)
		(width (array-dimension screens 1)))
	(setf (aref out-screens 121) (aref *screens* 121))
	(setf (aref out-screens 122) (aref *screens* 122))
	(mapc (lambda (coord)
			(when (eql current-index 121)
			  (incf current-index 2))
			(let ((screen (get-arr2d screens coord))
				  (ref-index (coord-2d-to-1d width coord)))
			  (if (numberp screen)
				(let ((out-index (cdr (assoc screen duplicate-nums))))
				  (if out-index
					(setf (aref screen-refs ref-index)
						  (+ (* (aref +monster-mixes+ ref-index) #x80)
							   out-index))
					(progn
					  (setf (aref out-screens current-index)
							(aref +init-duplicate-screens+ screen))
					  (setf (aref screen-refs ref-index)
							(+ (* (aref +monster-mixes+ ref-index) #x80)
							   current-index))
					  (push `(,screen . ,current-index) duplicate-nums)
					  (incf current-index))))
				(progn
				  (setf (aref out-screens current-index) screen)
				  (setf (aref screen-refs ref-index)
						(+ (* (aref +monster-mixes+ ref-index) #x80)
						   current-index))
				  (incf current-index)))))
		  (get-coords-list (array-dimension screens 1)
						   (array-dimension screens 0)))
	(mapc (lambda (x)
			(setf (aref rom-array (+ x #x18590)) (aref screen-refs x)))
		  (interval 0 127))
	(mapc (lambda (x)
			(mapc (lambda (col y)
					(setf (aref rom-array (+ #x15428 (* x #x10) y))
						  (filter-col-to-col-num col)))
				  (coerce (aref out-screens x) 'list)
				  (interval 0 15)))
		  (interval 0 123))))

(defun write-byte-coord (width rom-array index coord)
  (setf (aref rom-array index) (+ (* (car coord) width) (cadr coord))))

(defun coord-2d-to-1d (width coord)
  (+ (* (car coord) width) (cadr coord)))

(defun write-columns (rom-array byte-columns)
  (mapc (lambda (x)
		  (setf (aref rom-array (+ x #x15be8)) (aref byte-columns x)))
		(interval 0 963)))

(defun write-lost-screens (rom-array)
  (mapc (lambda (x) (setf (aref rom-array x) #xFF))
		'(#x6db7 #x6dd9)))

(defun write-palettes (rom-array screens base biomes palettes)
  (write-outer-palettes rom-array screens base biomes palettes)
  (write-inner-palettes rom-array palettes))

(defun write-outer-palettes (rom-array screens base biomes palettes)
  (mapc (lambda (index coord)
		  (setf (aref rom-array index)
				(+ (* (let ((base-scr (get-arr2d base coord)))
						(funcall
						  (if (< base-scr 0)
							(lambda (x) (- 15 x))
							#'identity)
						  (floor (aref +base-exit-cols+ (abs base-scr)) 16)))
					  16)
				   (if (and (intersection '(coast river-in river-out big-lake
											small-lake spring)
										  (get-arr2d biomes coord))
							(not (member 'island (get-arr2d biomes coord)))
							(not (numberp (get-arr2d screens coord)))
							(some
							  (lambda (col)
								(some
								  (lambda (tile)
									(member tile '(#x05 #x08 #x17 #x07 #x15 
												   #x09 #x18 #x06 #x16)))
								  col))
							  (mapcar (lambda (col) (column-by-number col))
									  (coerce (get-arr2d screens coord) 
											  'list))))
					 #x8 0)
				   (if (member 'coast (get-arr2d biomes coord))
					 #x4 0)
				   (cdr (assoc (car (get-arr2d palettes coord))
							   '((brown . 3) (green . 2) (orange . 1) 
											 (gray . 0)))))))
		(interval #x18410 #x1848f)
		(get-coords-list 16 8)))

(defun write-inner-palettes (rom-array palettes)
  (mapc (lambda (index coord)
		  (setf (aref rom-array index)
				(+ 4
				   (cdr (assoc (cadr (get-arr2d palettes coord))
							   '((brown . 3) (green . 2) (orange . 1) 
											 (gray . 0)))))))
		(interval #x18490 #x1850f)
		(get-coords-list 16 8)))

(defconstant +base-exit-cols+
			 (coerce
			   (append
				 (mapcar (lambda (x) (aref *byte-array* x))
						 (remove-if (lambda (x) 
									  (member (- x #x18410)
											  '(8 57 60 67 95 #x7a #x7d)
											  :test #'eql))
									(interval #x18410 #x1848f)))
				 '(#x00 #x00 #x62 #x0f #x0a #x0a #x0b #x0b #x3f #x0f #xdf #x5f 
				   #x3f #x70 #x70 #xc3))
			   'array))

(defconstant +base-exit-rows+
			 (coerce
			   (append
				 (mapcar (lambda (x) (aref *byte-array* x))
						 (remove-if (lambda (x) 
									  (member (- x #x18690)
											  '(8 57 60 67 95 #x7a #x7d)
											  :test #'eql))
									(interval #x18690 #x1870f)))
				 '(#x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x03 #x00 #x00 
				   #x00 #x00 #x04 #x04 #x04))
			   'array))

(defconstant +monster-mixes+
			 (coerce
			   (mapcar (lambda (x) (floor (aref *byte-array* x) #x80))
					   (interval #x18590 #x1860f))
			   'array))

(defun write-stair-and-exit-info (rom-array screens base)
  (mapc (lambda (index coord)
		  (setf (aref rom-array index)
				(mod (aref +base-exit-rows+ (abs (get-arr2d base coord))) 64)))
		(interval #x18690 #x1870f)
		(get-coords-list (array-dimension base 1) (array-dimension base 0))))

(defun write-whistle-info (rom-array base biomes)
  (let* ((pond-screens (remove-if (lambda (x) (not (member 
													'pond 
												    (get-arr2d biomes x))))
								 (get-coords-list (array-dimension base 1)
												  (array-dimension base 0))))
		 (other-screens (remove-if (lambda (x)
									 (or (member x pond-screens :test #'equal)
										 (not (member (get-arr2d base x)
													 '(6 40 42 47 57 84 91 105 
												       109 123 136)))))
								  (get-coords-list (array-dimension base 1)
												   (array-dimension base 0)))))
	(mapc (lambda (coord index)
			(push coord *final-whistle-screens*)
			(write-byte-coord (array-dimension base 1) rom-array index coord))
		  (append pond-screens (shuffle-list other-screens))
		  (interval #x1ef76 #x1ef80)))
  (reverse *final-whistle-screens*))

(defun write-raft-info (rom-array screens horiz-edges vert-edges)
  (let ((dock-screens (remove-if (lambda (coord)
								   (not (eq (north-edge horiz-edges vert-edges
												        (car coord) 
														(cadr coord))
									        'dock)))
								 (get-coords-list 
								   (array-dimension screens 1)
								   (array-dimension screens 0)))))
	(mapc (lambda (x) (write-byte-coord (array-dimension screens 1) rom-array
										x (first dock-screens)))
		  '(#x14954 #x10f94))
	(write-byte-coord (array-dimension screens 1) rom-array
					  #x14958 (second dock-screens))
	(mapc 
	  (lambda (y scr-coord)
		(setf (aref rom-array y) 
			  (* (find-if (lambda (x) 
							(member (aref (get-arr2d screens scr-coord) x)
									'(dock-open dock-wall)))
						  (interval 0 (1- (length 
											(get-arr2d screens scr-coord)))))
				 16)))
	  '(#x10f90 #x10f98) dock-screens)))

;a placeholder until a properly implement secret passages.
;For now, makes it so the puzzle solved jingle doesn't play when you enter
;screen 0f
(defun write-secret-passage (rom-array secret-passage)
  (setf (aref rom-array #x1ea9c) #xff))

(defun choose-stepladder-screens (rom-array vertex-array screens base biomes 
											horiz-edges vert-edges)
  (let* ((coast-heart-coord (write-coast-heart rom-array screens base biomes
											horiz-edges vert-edges))
		 (right-bridge-coord (find-if
							   (lambda (x) (eql (get-arr2d base x) 131))
							   (get-coords-list (array-dimension base 1)
												(array-dimension base 0))))
		 (crossing-screens
		   (remove-if (lambda (x)
						(not (and (member 'river-in (get-arr2d biomes x))
								  (listp (get-arr2d vertex-array x)))))
					  (get-coords-list (array-dimension base 1)
									   (array-dimension base 0))))
		 (cave-screens
		   (remove-if (lambda (x)
						(or (member x crossing-screens :test #'equal)
							(numberp (get-arr2d screens x))
							(not (member 212 (coerce (get-arr2d screens x) 
													 'list)))))
					  (get-coords-list (array-dimension base 1)
									   (array-dimension base 0))))
		 (extra-screens
		   (remove-if (lambda (x)
						(or (equal x coast-heart-coord)
							(not (eql (abs (get-arr2d base x)) 75))))
					  (get-coords-list (array-dimension base 1)
									   (array-dimension base 0)))))
	(mapc (lambda (coord index)
			(push coord *final-ladder-screens*)
			(write-byte-coord (array-dimension base 1) rom-array index coord))
		  (append `(,coast-heart-coord)
				  (if right-bridge-coord `(,right-bridge-coord) nil)
				  crossing-screens
				  cave-screens
				  extra-screens)
		  (interval #x1f21d #x1f222))))

(defun write-coast-heart (rom-array screens base biomes horiz-edges vert-edges)
  (let ((left-bridge-coord (find-if 
							 (lambda (x) (eql (get-arr2d base x) 130))
							 (get-coords-list (array-dimension base 1)
											  (array-dimension base 0))))
		(normal-opts (remove-if (lambda (x) 
								  (not (and (eql (abs (get-arr2d base x)) 75)
									        (member 'coast 
												    (get-arr2d biomes x)))))
							    (get-coords-list (array-dimension base 1)
											     (array-dimension base 0))))
		(backup-opts (remove-if (lambda (x) 
								(not
								  (and
								    (member 'water-wall
										    `(,(west-edge horiz-edges vert-edges
												  		  (car x) (cadr x))
											  ,(east-edge horiz-edges vert-edges
														  (car x) (cadr x))))
								    (not (intersection 
										   '(water-wall dock)
										   `(,(north-edge horiz-edges vert-edges
														  (car x) (cadr x))
										     ,(south-edge horiz-edges vert-edges
														  (car x) (cadr x)))))
								    (member 'coast (get-arr2d biomes x)))))
								(get-coords-list (array-dimension base 1)
												 (array-dimension base 0)))))
	(let ((coord-choice (cond (left-bridge-coord left-bridge-coord)
							  (normal-opts (nth (mrandom (length normal-opts))
												normal-opts))
							  (backup-opts 
								(let ((opt (nth (mrandom (length backup-opts))
												backup-opts)))
								  (mapc (lambda (x)
										  (setf (aref (get-arr2d screens opt) x)
												18))
										(if (eq (east-edge horiz-edges 
															vert-edges
															(car opt)
															(cadr opt))
												'water-wall)
										  '(10 12) '(3 5)))
								  opt))
							  (t (mrandom 0)))))
	  (write-byte-coord (array-dimension base 1) rom-array 
						#x178aa coord-choice)
	  (unless (eql (aref (get-arr2d screens coord-choice) 12) 18)
		(setf (aref rom-array #x1789e) #x30))
	  coord-choice)))

(defun write-indoor-palette-loc (rom-array biomes)
  (write-byte-coord (array-dimension biomes 1) rom-array #x1717c
					(find-if (lambda (coord) 
							   (member 'a-fairy (get-arr2d biomes coord)))
							 (get-coords-list (array-dimension biomes 1)
											  (array-dimension biomes 0)))))
