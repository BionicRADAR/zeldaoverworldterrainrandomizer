;;adapt-screens.lisp
;;Part of the zelda overworld randomizer
;;Handles altering the individual columns in the already-placed screens
;;so they fit together. Dependent on the biomes already being placed,
;;the edges already being decided, the screens being put into place,
;;and the columns being altered already. Except for any last-minute
;;emergency changes to ensure that the whole overworld is accessible,
;;and ensuring the correct number of caves in the game
;;this should be the last step in actually generating the overworld.
;;Makes use of normal-biome-exits.lisp, which handles the logic for
;;adapting between two normal biome screens (forest, woods, mountain, hills)
;;@author Nathan Schleicher


(defun adapt-screens (width height biomes horiz vert screens base
							spec-screens spec-biome-info)
  (fill-cols-not-to-replace)
  (simplify-screens-trees screens
						  (let ((trees-array (make-array '(8 16))))
							(mapc (lambda (x)
									(setf (aref trees-array (car x) (cadr x))
										  (intersection '(forest woods)
														(aref biomes (car x)
															  (cadr x)))))
								  (get-coords-list 16 8))
							trees-array))
  (adapt-presets (remove-if (lambda (x)
								 (not (or (numberp (aref screens 
														 (car x) (cadr x)))
										  (eql (aref base (car x) (cadr x))
											   33))))
							(get-coords-list width height))
					screens base biomes horiz vert)
  (adapt-grave (cdr (assoc 'graveyard spec-screens))
			   (cdr (assoc 'grave spec-biome-info))
			   screens base biomes horiz vert)
  (adapt-desert (cdr (assoc 'desert spec-screens))
				(cdr (assoc 'desert spec-biome-info))
				screens base biomes horiz vert)
  (adapt-islands (cdr (assoc 'island spec-screens))
				 screens base biomes horiz vert)
  (mapc (lambda (x) (push x *cols-not-to-replace*))
		'(high-river 115 ladder-river 176))
  (adapt-river-in (cdr (assoc 'river-in spec-screens))
				  (cadr (assoc 'river-in spec-biome-info))
				  screens base biomes horiz vert)
  (mapc (lambda (x) (when (eq (aref horiz (car x) (cadr x)) 'bridge)
					  (set-adapted-side x `(,(car x) ,(1+ (cadr x))) 'adapted)))
		(get-coords-list 15 8))
  (adapt-remaining screens base biomes horiz vert)
  (final-connect-map screens base biomes *connect-graph-array* horiz vert)
  (finalize-simplifications))

(defun adapt-remaining (screens base biomes horiz vert)
  (mapc 
	(lambda (coord)
	  (mapc (lambda (other side edge)
			(let ((left (if (eq side 'west) other coord))
				  (right (if (eq side 'west) coord other)))
			  (adapt-if-needed 
				left right 'horiz
				(lambda ()
				  (preserve-vert-adapt-side
					(get-all-adapted left) (get-all-adapted right)
					(aref base (car left) (cadr left))
					(aref base (car right) (cadr right))
					(aref screens (car left) (cadr left))
					(aref screens (car right) (cadr right))
					(eq edge 'edge)
					*extends*))
				edge screens base)))
			(mapcar (lambda (x) `(,(car coord) ,(+ x (cadr coord)))) '(-1 1))
			'(west east)
			(mapcar (lambda (x) (funcall x horiz vert (car coord) (cadr coord)))
					(list #'west-edge #'east-edge))))
	(get-coords-list 16 8))
  (mapc
	(lambda (coord)
	  (mapc (lambda (other side edge)
			  (let ((top (if (eq side 'north) other coord))
					(bot (if (eq side 'south) other coord)))
				(adapt-if-needed 
				  top bot 'vert
				  (if (eq edge 'edge)
					(lambda ()
					  (adapt-vert (aref base (car top) (cadr top))
								  (aref base (car bot) (cadr bot))
								  (aref screens (car top) (cadr top))
								  (aref screens (car bot) (cadr bot))
								  *extends*))
					(lambda ()
					  (adapt-vert-to-wall (aref screens (car top) (cadr top))
										  (aref base (car top) (cadr top))
										  'bot)
					  (adapt-vert-to-wall (aref screens (car bot) (cadr bot))
										  (aref base (car bot) (cadr bot))
										  'top)))
				  edge screens base)))
			(mapcar (lambda (x) `(,(+ x (car coord)) ,(cadr coord))) '(-1 1))
			'(north south)
			(mapcar (lambda (x) (funcall x horiz vert (car coord) (cadr coord)))
					(list #'north-edge #'south-edge))))
	(get-coords-list 16 8)))

(defun adapt-islands (island-screens screens base biomes horiz vert)
  (mapc (lambda (coord)
		  (cond ((eq (west-edge horiz vert (car coord) (cadr coord)) 'bridge)
				 (mapc (lambda (x)
						 (setf (aref 
								 (aref screens (car coord) (1- (cadr coord)))
								 x) 18))
					   (interval 10 15))
				 (setf (aref (aref screens (car coord) (1- (cadr coord))) 9)
					   (if (or (eq (south-edge horiz vert (car coord)
											   (1- (cadr coord))) 'water-wall)
							   (member 'river-in (aref biomes (car coord)
													   (1- (cadr coord)))))
						 18 68)))
				((eq (east-edge horiz vert (car coord) (cadr coord)) 'bridge)
				 (mapc (lambda (x)
						 (setf (aref 
								 (aref screens (car coord) (1+ (cadr coord)))
								 x) 18))
					   (interval 0 5))
				 (setf (aref (aref screens (car coord) (1+ (cadr coord))) 6)
					   (if (or (eq (south-edge horiz vert (car coord)
											   (1+ (cadr coord))) 'water-wall)
							   (member 'river-in (aref biomes (car coord)
													   (1+ (cadr coord)))))
						 18 68)))
				((member 'big-lake (aref biomes (car coord) (cadr coord)))
				 (setf (aref (aref screens (1+ (car coord)) (cadr coord)) 
							 (if (eq (east-edge horiz vert 
												(1+ (car coord)) (cadr coord))
									 'water-wall)
							   7 8))
					   (if (eq (south-edge horiz vert (1+ (car coord)) 
										   (cadr coord)) 'edge)
							   (simplify-column 'dock-open) 'dock-wall))))
		  (mapc (lambda (other side)
				  (unless (or (not (valid-coordp 16 8 other))
							  (numberp (get-arr2d screens other))
							  (member 'coast
									  (aref biomes (car other) (cadr other))))
					(if (member side '(bot top))
					  (adapt-vert-to-wall 
						(aref screens (car other) (cadr other))
						(aref base (car other) (cadr other)) side)
					  (adapt-to-special
						(aref screens (car other) (cadr other))
						(aref base (car other) (cadr other))
						0 side))))
				(mapcar (lambda (x) `(,(+ (car x) (car coord)) 
									  ,(+ (cadr x) (cadr coord))))
						'((-1 0) (1 0) (0 -1) (0 1)))
				'(bot top right left))
		  (set-adapted-vert `(,(1- (car coord)) ,(cadr coord)) coord 'adapted)
		  (set-adapted-vert coord `(,(1+ (car coord)) ,(cadr coord)) 'adapted)
		  (set-adapted-side `(,(car coord) ,(1- (cadr coord))) coord 'adapted)
		  (set-adapted-side coord `(,(car coord) ,(1+ (cadr coord))) 'adapted))
		island-screens))

(defun adapt-vert-to-wall (screen base vert)
  (mapc (lambda (x) (remove-edge screen base x vert))
		(interval 0 15)))

(defun adapt-river-in (river-in-screens river-in-info screens base biomes
										horiz vert)
  (let ((mouth (cadddr river-in-info))
		(neighbor (caddr river-in-info))
		(direction (car river-in-info))
		(distance (cadr river-in-info)))
	(when (not (eql (car mouth) (car neighbor)))
	  (when (eql distance 2)
		(mapc (lambda (x) 
				(unless (cdr (assoc 'ladder-river *column-names*))
				  (setf (aref (aref screens (car neighbor) 
											(cadr neighbor))
									  x) 'high-river)))
			  (if (eq direction 'left) '(0 1) '(14 15))))
	  (adapt-if-needed (if (< (car mouth) (car neighbor)) mouth neighbor)
					   (if (< (car mouth) (car neighbor)) neighbor mouth)
					   'vert
					   (lambda ()
						 (let ((open-lst
								 (if (< (car mouth) (car neighbor))
								   (if (eq direction 'left)
									 '(7 8 9 10 11) '(4 5 6 7 8))
								   (if (eq direction 'left)
									 '(8 7 6 5) '(7 8 9 10))))
							   (neighbor-edge-num
								 (if (< (car mouth) (car neighbor))
								   0 10)))
						   (mapc (lambda (x)
								   (if (and
										 (member x open-lst :test #'eql)
										 (passable
										   (nth neighbor-edge-num
												(column-by-number
												  (aref
													(get-arr2d 
													  screens neighbor) x)))))
									 (add-vert-open 
									   (aref screens (car mouth) (cadr mouth))
									   (aref base (car mouth) (cadr mouth))
									   x
									   (if (< (car mouth) (car neighbor))
										 'bot 'top))
									 (remove-edge
									   (get-arr2d screens mouth)
									   (get-arr2d base mouth)
									   x
									   (if (< (car mouth) (car neighbor))
										 'bot 'top))))
								 (interval 0 15))))
					   (funcall (if (< (car mouth) (car neighbor))
								  #'south-edge #'north-edge)
								horiz vert (car mouth) (cadr mouth))
					   screens base)
	  (let* ((bend-vert `(,(+ (if (< (car mouth) (car neighbor)) 1 -1) 
							  (car neighbor)) ,(cadr neighbor)))
			 (edge (funcall (if (< (car mouth) (car neighbor))
							  #'south-edge #'north-edge)
						    horiz vert (car neighbor) (cadr neighbor)))
			 (top (if (< (car mouth) (car neighbor)) neighbor bend-vert))
			 (bot (if (< (car mouth) (car neighbor)) bend-vert neighbor)))
		(adapt-if-needed (if (< (car mouth) (car neighbor)) neighbor bend-vert)
						 (if (< (car mouth) (car neighbor)) bend-vert neighbor)
						 'vert
						 (if (eq edge 'edge)
						   (lambda ()
							 (adapt-vert 
							   (get-arr2d base top)
							   (get-arr2d base bot)
							   (get-arr2d screens top)
							   (get-arr2d screens bot)
							   *extends*))
						   (lambda ()
							 (adapt-vert-to-wall 
							   (get-arr2d screens top)
							   (get-arr2d base top) 'bot)
							 (adapt-vert-to-wall 
							   (get-arr2d screens bot)
							   (get-arr2d base bot) 'top)))
						 edge screens base)))
	(mapc
	  (lambda (coord)
		(mapc
		  (lambda (other side)
			(if (and (valid-coordp 16 8 other)
					 (member 'river-in (aref biomes (car other) (cadr other))))
			  (funcall (if (member side '(north south))
						 #'set-adapted-vert #'set-adapted-side)
					   (if (member side '(north west)) other coord)
					   (if (member side '(north west)) coord other)
					   'adapted)
			  (if (and (valid-coordp 16 8 other)
					   (member 'spring (aref biomes (car other) (cadr other))))
				(set-adapted-vert other coord 'adapted)
				(when (member side '(north south))
				  (adapt-if-needed (if (eq side 'north) other coord)
								   (if (eq side 'north) coord other)
								   'vert
								   (lambda ()
									 (mapc (lambda (x)
											 (remove-edge
											   (aref screens (car other)
													 (cadr other))
											   (aref base (car other)
													 (cadr other))
											   x
											   (if (eq side 'north) 'bot 'top)))
										   (interval 0 15)))
								   (funcall (if (eq side 'north)
											  #'north-edge #'south-edge)
											horiz vert
											(car coord) (cadr coord))
								   screens base)))))
		  (mapcar (lambda (x) `(,(+ (car x) (car coord)) 
								,(+ (cadr x) (cadr coord))))
				  '((-1 0) (1 0) (0 -1) (0 1)))
		  '(north south west east)))
	  river-in-screens)))


(defun adapt-desert (desert-screens desert-info screens base biomes horiz vert)
  (labels ((adapt-horiz (coord other side edge-type)
						(mapc (lambda (x)
								(setf 
								  (aref (aref screens (car coord) (cadr coord))
										x)
								  (if (eql edge-type 0) 0 249)))
							  (if (eq side 'west)
								'(0 1) '(14 15)))
						(adapt-to-special 
						  (aref screens (car other) (cadr other))
						  (aref base (car other) (cadr other))
						  edge-type (if (eq side 'west) 'right 'left))))
	(let* ((offshoot-type (cond ((eql (caadr desert-info) -1) 'north)
								((eql (caadr desert-info) 2) 'south)
								((eql (cadadr desert-info) -1) 'west)
								((eql (cadadr desert-info) 2) 'east)))
		   (neighbor (if (member offshoot-type '(north south))
					   `(,(+ (if (eq offshoot-type 'north) 0 1) 
							 (caar desert-info))
						 ,(+ (cadadr desert-info) (cadar desert-info)))
					   `(,(+ (caadr desert-info) (caar desert-info))
						 ,(+ (if (eq offshoot-type 'west) 0 1)
							 (cadar desert-info))))))
	  (if (member offshoot-type '(north south))
		(mapc (lambda (x)
				(add-vert-open
				  (aref screens (car neighbor) (cadr neighbor))
				  (aref base (car neighbor) (cadr neighbor))
				  x (if (eq offshoot-type 'north) 'top 'bot)))
			  (interval 2 13))
		(adapt-to-special (aref screens (car neighbor) (cadr neighbor))
						  (aref base (car neighbor) (cadr neighbor))
						  4 (if (eq offshoot-type 'west) 'left 'right))))
	(mapc (lambda (coord other side edge)
			(if (and (valid-coordp 16 8 other)
					 (member 'desert (aref biomes (car other) (cadr other))))
			  (funcall (if (member side '(north south))
						 #'set-adapted-vert #'set-adapted-side)
					   (if (member side '(north west)) other coord)
					   (if (member side '(north west)) coord other)
					   'adapted)
			  (adapt-if-needed
				(if (member side '(north west)) other coord)
				(if (member side '(north west)) coord other)
				(if (member side '(north south)) 'vert 'horiz)
				(if (member side '(north south))
				  (if (eq edge 'edge)
					(lambda ()
					  (let ((scr1 (if (eq side 'north) other coord))
							(scr2 (if (eq side 'north) coord other)))
						(adapt-vert (aref base (car scr1) (cadr scr1))
									(aref base (car scr2) (cadr scr2))
									(aref screens (car scr1) (cadr scr1))
									(aref screens (car scr2) (cadr scr2))
									*extends*)))
					(lambda ()
					  (mapc (lambda (x y)
							  (adapt-vert-to-wall 
								(aref screens (car x) (cadr x))
								(aref base (car x) (cadr x)) y))
							(list (if (eq side 'north) other coord)
								  (if (eq side 'north) coord other))
							'(bot top))))
				  (lambda ()
					(adapt-horiz coord other side (if (eq edge 'edge) 4 0))))
				edge screens base)))
		  (apply #'append (mapcar (lambda (x) (list x x x x)) desert-screens))
		  (apply #'append 
				 (mapcar 
				   (lambda (x)
					 (mapcar (lambda (y) `(,(+ (car x) (car y))
										   ,(+ (cadr x) (cadr y))))
							 '((-1 0) (1 0) (0 -1) (0 1))))
				   desert-screens))
		  (apply #'append 
				 (mapcar (lambda (x) '(north south west east)) desert-screens))
		  (apply #'append
				 (mapcar (lambda (x)
						   (mapcar (lambda (y)
									 (funcall y horiz vert (car x) (cadr x)))
								   (list #'north-edge #'south-edge #'west-edge
										 #'east-edge)))
						 desert-screens)))))

(defun cardinal-to-relative (direction)
  (cdr (assoc direction 
			  '((north . top) (south . bot) (west . left) (east . right)))))


(defun adapt-grave (grave-screens grave-info screens base biomes horiz vert)
  (labels ((adapt-horiz (coord other side edge-type)
						  (mapc (lambda (x side)
								  (adapt-to-special (aref screens (car x)
														  (cadr x))
													(aref base (car x) (cadr x))
													edge-type side))
								`(,coord ,other)
								`(,side ,(if (eq side 'left) 'right 'left))))
		   (handle-horiz (coord other edge side)
					   (if (and (valid-coordp 16 8 other)
								(member 'graveyard 
										(aref biomes (car other) (cadr other))))
						 (set-adapted-side
						   (if (eq side 'west) other coord)
						   (if (eq side 'west) coord other)
						   'adapted)
						 (adapt-if-needed
						   (if (eq side 'west) other coord)
						   (if (eq side 'west) coord other)
						   'horiz
						   (lambda () (adapt-horiz coord other 
												   (cardinal-to-relative side)
												   (if (eq edge 'wall) 0 9)))
						   edge screens base)))
		   (handle-vert (coord other edge side)
						(if (and (valid-coordp 16 8 other)
								 (member 'graveyard
									 (aref biomes (car other) (cadr other))))
						  (set-adapted-vert
							(if (eq side 'north) other coord)
							(if (eq side 'north) coord other)
							'adapted)
						  (adapt-if-needed
							(if (eq side 'north) other coord)
							(if (eq side 'north) coord other)
							'vert
							(if (eq edge 'edge)
							  (lambda () (add-vert-grave-edge coord other side))
							  (lambda () (mapc
								(lambda (x vert)
								  (mapc (lambda (y)
										  (remove-edge (aref screens (car x)
															 (cadr x))
													   (aref base (car x)
															 (cadr x))
													   y vert))
										(interval 0 15)))
								  `(,coord ,other)
								  (if (eq side 'north) 
									'(top bot) '(bot top)))))
							  edge screens base)))
		   (alter-vert (tcoord bcoord cols tile)
					   (mapc (lambda (x vert)
							   (mapc (lambda (y)
									   (unless 
										 (and (member y cols)
											  (eql 
												(nth
												  (if (eq vert 'bot) 10 0)
												  (column-by-number
													(aref (aref screens (car x)
																(cadr x))
														  y)))
												tile))
										 (remove-edge
											(aref screens (car x) (cadr x))
											(aref base (car x) (cadr x))
											y vert)))
									 (interval 0 15))
							   (mapc (lambda (y)
									   (extend-screen
										 (aref screens (car x) (cadr x))
										 (aref base (car x) (cadr x))
										 *extends* y)
									   (funcall (if (eq tile #x0A) #'add-ladder
												  #'add-vert-open)
												(aref screens (car x) (cadr x))
												(aref base (car x) (cadr x))
												y vert))
									 cols))
							 `(,tcoord ,bcoord) '(bot top)))
		   (add-vert-grave-edge (coord other side)
					   (let ((replaceables
							   (remove-if (lambda (x)
											(member (aref
													  (aref screens (car other)
															(cadr other))
													  x)
													*cols-not-to-replace*))
										  (interval 0 15))))
							 (if (and (intersection '(mountain hills) 
											 (aref biomes (car other) 
												   (cadr other)))
									  (some
										(lambda (x)
										  (and (member x replaceables)
											   (eql (aref (aref screens
																(car coord)
																(cadr coord))
														  x) 2)))
										'(1 14)))
							   (let* ((opts
									   (remove-if
										 (lambda (x)
										   (not
											 (and (member x replaceables)
											   (eql (aref (aref screens
																(car coord)
																(cadr coord))
														  x) 2))))
										 '(1 14)))
									  (pick (nth (mrandom (length opts)) opts)))
								 (alter-vert (if (eq side 'north) other coord)
											 (if (eq side 'north) coord other)
											 `(,pick) #x0A))
							   (let* ((opts
									   (remove-if
										 (lambda (x)
										   (not (every
												  (lambda (y)
													(member y replaceables))
												  x)))
										 '((4 5) (7 8) (10 11))))
									  (pick (if opts (nth 
													   (mrandom (length opts))
													   opts)
											  nil)))
								 (if pick
								   (alter-vert (if (eq side 'north) other coord)
											   (if (eq side 'north) coord other)
											   pick #x0E)
								   (let ((scr1 (if (eq side 'north) 
												 other coord))
										 (scr2 (if (eq side 'north)
												 coord other)))
									 (mixed-vert-adapt
										(aref screens (car scr1) (cadr scr1))
										(aref base (car scr1) (cadr scr1))
										(aref screens (car scr2) (cadr scr2))
										(aref base (car scr2) (cadr scr2))
										*extends*))))))))
	(mapc #'handle-horiz
		  (append grave-screens grave-screens)
		  (append (mapcar (lambda (x) `(,(car x) ,(1- (cadr x)))) grave-screens)
				  (mapcar (lambda (x) `(,(car x) ,(1+ (cadr x)))) 
						  grave-screens))
		  (append (mapcar (lambda (x) (west-edge horiz vert (car x) (cadr x)))
						  grave-screens)
				  (mapcar (lambda (x) (east-edge horiz vert (car x) (cadr x)))
						  grave-screens))
		  (append (mapcar (lambda (x) 'west) grave-screens)
				  (mapcar (lambda (x) 'east) grave-screens)))
	(mapc #'handle-vert
		  (append grave-screens grave-screens)
		  (append (mapcar (lambda (x) `(,(1- (car x)) ,(cadr x))) grave-screens)
				  (mapcar (lambda (x) `(,(1+ (car x)) ,(cadr x)))
						  grave-screens))
		  (append (mapcar (lambda (x) (north-edge horiz vert (car x) (cadr x)))
						  grave-screens)
				  (mapcar (lambda (x) (south-edge horiz vert (car x) (cadr x)))
						  grave-screens))
		  (append (mapcar (lambda (x) 'north) grave-screens)
				  (mapcar (lambda (x) 'south) grave-screens)))))



(defun adapt-presets (presets screens base biomes horiz vert)
  (flet ((handle-one-preset (coord)
							(mapc 
							  (lambda (other dir edge coord-first-p)
								(unless 
								  (or (member other presets :test #'equal))
								  (adapt-if-needed
									(if coord-first-p coord other)
									(if coord-first-p other coord)
									dir 
									(if (eq dir 'vert)
									  (lambda ()
										(special-vert-adapt
										  coord other screens base biomes
												 vert))
									  (lambda ()
										(adapt-to-special
										  (aref screens (car other) 
												(cadr other))
										  (aref base (car other) (cadr other))
										  (funcall
											(if coord-first-p #'right-type
											  #'left-type)
											(aref base (car coord) 
												  (cadr coord)))
										  (if coord-first-p 'left 'right))))
									edge screens base))
								(funcall
								  (if (eq dir 'vert) #'set-adapted-vert
									#'set-adapted-side)
								  (if coord-first-p coord other)
								  (if coord-first-p other coord)
								  'adapted))
							  (mapcar (lambda (x y) `(,(+ x (car coord))
													  ,(+ y (cadr coord))))
									  '(-1 1 0 0) '(0 0 -1 1))
							  '(vert vert horiz horiz)
							  (mapcar (lambda (x)
										(funcall x horiz vert (car coord)
												 (cadr coord)))
									  (list #'north-edge #'south-edge
											#'west-edge #'east-edge))
							  '(nil t nil t))))
	(mapc #'handle-one-preset presets)))


(defun special-vert-adapt (source-coord change-coord screens base biomes
										vert-edges)
  (when (valid-coordp 16 8 change-coord)
  (let* ((change-side (if (< (car source-coord) (car change-coord)) 'top 'bot))
		 (source-side (if (eq change-side 'top) 'bot 'top))
		 (vert-edge (passable-edge-p vert-edges
									 (car (if (eq source-side 'top) 
											change-coord source-coord))
									 (cadr source-coord)))
		 (ladders (if vert-edge (find-ladder-cols
								  (let ((screen (aref screens (car source-coord)
													  (cadr source-coord))))
									(if (numberp screen)
									  (aref +init-duplicate-screens+ screen)
									  screen))
								  source-side) nil))
		 (opens (if vert-edge (find-open-cols
								(let ((screen (aref screens (car source-coord) 
													(cadr source-coord))))
								  (if (numberp screen)
									(aref +init-duplicate-screens+ screen)
									screen))
								  source-side) nil)))
	(mapc (lambda (x) (remove-edge (aref screens (car change-coord)
										 (cadr change-coord))
								   (aref base (car change-coord) 
										 (cadr change-coord))
								   x change-side))
		  (remove-if (lambda (x) (member x (append ladders opens)))
					 (interval 0 15)))
	(when vert-edge
	  (progn (when ladders (mapc (lambda (x)
								   (special-ladder-add change-coord screens
													   base biomes x 
													   change-side))
								 ladders))
			 (when opens (mapc (lambda (x)
								 (extend-screen (aref screens
													  (car change-coord)
													  (cadr change-coord))
												(aref base
													  (car change-coord)
													  (cadr change-coord))
												*extends*
												x)
								 (add-vert-open (aref screens
													  (car change-coord)
													  (cadr change-coord))
												(aref base
													  (car change-coord)
													  (cadr change-coord))
												x
												change-side))
							   opens)))))))

(defun special-ladder-add (change-coord screens base biomes col-num vert)
  (extend-screen (aref screens (car change-coord) (cadr change-coord))
				 (aref base (car change-coord) (cadr change-coord))
				 *extends* col-num)
  (if (and (member 'graveyard 
				   (aref biomes (car change-coord) (cadr change-coord)))
		   (eql vert 'top))
	(setf (aref (aref screens (car change-coord) (cadr change-coord)) col-num)
		  (if (cdr (assoc 'ladder-open *column-names*))
			'ladder-open 24))
	(if (member 'desert (aref biomes (car change-coord) (cadr change-coord)))
	  (if (eq vert 'both)
		(setf (aref (aref screens (car change-coord) (cadr change-coord)) 
					col-num) 197)
		(add-vert-open (aref screens (car change-coord) (cadr change-coord))
					   (aref base (car change-coord) (cadr change-coord))
					   col-num vert))
	  (if (intersection '(forest woods) 
						(aref biomes (car change-coord) (cadr change-coord)))
		(add-vert-open (aref screens (car change-coord) (cadr change-coord))
					   (aref base (car change-coord) (cadr change-coord))
					   col-num vert)
		(add-ladder (aref screens (car change-coord) (cadr change-coord))
					(aref base (car change-coord) (cadr change-coord))
					col-num vert)))))

(defconstant +desert-columns+ '(196 197 198 249))

(defun preserve-vert-adapt-side (left-adapted right-adapted base-left 
											  base-right left right has-edge 
											  extends)
  (flet ((return-ladder (screen col-num vert)
						(let ((cols (list (1- col-num) col-num (1+ col-num))))
						  (mapc (lambda (x y)
								  (when (and y (not (member x '(0 15))))
									(setf (aref screen x) y)))
								cols
								(if (eq vert 'both)
								  (apply #'pick-double-ladders cols)
								  (apply #'pick-normal-ladder-col
										 (append cols `(,vert)))))))
		 (return-open (screen base-scr col-num vert)
					  (if (eq vert 'both)
						(setf (aref screen col-num)
								(if (member (aref screen col-num)
											+desert-columns+)
								  197 24))
						(add-vert-open screen base-scr col-num vert)))
		 (get-preserves (screen side adapted)
						(list
						  (mapcar (lambda (x)
									(cond ((and (car adapted) (cadr adapted)
											 (eql (aref screen x) 
												  *double-ladder*))
										   `(,x both))
										  ((and (car adapted)
												(eql (nth 0 
														  (new-column-by-number
															(aref screen x)))
													 #x0A))
										   `(,x top))
										  ((and (cadr adapted)
												(eql (nth 10 
														  (new-column-by-number
															(aref screen x)))
													 #x0A))
										   `(,x bot))
										  (t nil)))
								  (if (eq side 'left)
									'(13 14) '(1 2)))
						  (let ((x (if (eq side 'left) 14 1)))
							(cond ((and (car adapted) (cadr adapted)
										(member (aref screen x) '(24 197)))
								   `(,x both))
								  ((and (car adapted)
										(eql (nth 0 (new-column-by-number
													  (aref screen x))) #x0E))
								   `(,x top))
								  ((and (cadr adapted)
										(eql (nth 10 (new-column-by-number
													   (aref screen x))) #x0E))
								   `(,x bot))
								  (t nil))))))
	(let ((left-preserves (get-preserves left 'left left-adapted))
		  (right-preserves (get-preserves right 'right right-adapted)))
	  (adapt-side base-left base-right left right has-edge extends)
	  (mapc (lambda (x)
			  (when x (return-ladder left (car x) (cadr x))))
			(car left-preserves))
	  (mapc (lambda (x)
			  (when x (return-ladder right (car x) (cadr x))))
			(car right-preserves))
	  (when (cadr left-preserves) 
		(return-open left base-left (caadr left-preserves) 
					 (cadadr left-preserves)))
	  (when (cadr right-preserves) 
		(return-open right base-right (caadr right-preserves) 
					 (cadadr right-preserves))))))


(defun get-all-adapted (coord)
  (aref *edges-adapted* (car coord) (cadr coord)))

(defun get-adapted (coord side)
  (nth (position side '(north south west east))
	   (aref *edges-adapted* (car coord) (cadr coord))))

(defun set-adapted (coord side new-val)
  (setf (nth (position side '(north south west east))
			 (aref *edges-adapted* (car coord) (cadr coord)))
		new-val))

(defun set-adapted-side (l-coord r-coord new-val)
  (when (valid-coordp 16 8 l-coord)
	(setf (nth 3 (aref *edges-adapted* (car l-coord) (cadr l-coord))) new-val))
  (when (valid-coordp 16 8 r-coord)
	(setf (nth 2 (aref *edges-adapted* (car r-coord) (cadr r-coord))) new-val)))

(defun set-adapted-vert (t-coord b-coord new-val)
  (when (valid-coordp 16 8 t-coord)
	(setf (nth 1 (aref *edges-adapted* (car t-coord) (cadr t-coord))) new-val))
  (when (valid-coordp 16 8 b-coord)
	(setf (nth 0 (aref *edges-adapted* (car b-coord) (cadr b-coord))) new-val)))

;Assumes at least one input coordinate is valid.
(defun adapt-if-needed (tl-coord br-coord dir adapt-fun edge screens base)
  (if (not (eq edge 'none))
	(when (or (not (get-adapted tl-coord (if (eq dir 'vert) 'south 'east)))
			  (not (get-adapted br-coord (if (eq dir 'vert) 'north 'west))))
	  (unless (eq edge 'water-wall) (funcall adapt-fun))
	  (if (eq dir 'vert)
		(set-adapted-vert tl-coord br-coord 'adapted)
		(set-adapted-side tl-coord br-coord 'adapted)))
	(let* ((valid (if (valid-coordp 16 8 tl-coord) tl-coord br-coord))
		   (side (if (eq dir 'horiz)
				   (if (equal valid tl-coord) 'right 'left)
				   (if (equal valid tl-coord) 'bot 'top)))
		   (card-side (cdr (assoc side '((top . north) (bot . south)
										 (left . west) (right . east)))))
		   (scr (aref screens (car valid) (cadr valid)))
		   (base-scr (aref base (car valid) (cadr valid))))
	  (when (not (get-adapted valid card-side))
		(when (not (numberp (aref screens (car valid) (cadr valid))))
		  (if (eq dir 'vert)
			(mapc (lambda (x) (remove-edge scr base-scr x side)) 
				  (interval 0 15))
			(adapt-to-special scr base-scr 0 side)))
		(set-adapted valid card-side 'adapted)))))

(defsetf get-adapted set-adapted)

(defun fill-cols-not-to-replace ()
  (mapc #'add-col-not-to-replace
		(append (unless (cdr (assoc 'bl-open *column-names*)) 
				  '(bl-rock bl-tree))
				(unless (cdr (assoc 'br-open *column-names*)) 
				  '(br-rock br-tree)))))

(defconstant +base-split-screens+ '(22 28 34 71 88 93 103 104 109))

(defconstant +vert-split-screen-replacements+
			 '((87 . 118) (48 . 23) (116 . 35)))

(defconstant +vert-split-screen-connectors+
			 '((88 (1 2 3 4)) (109 (6 7 8 9 10 11 12 13 14 15))))

(defconstant +horiz-split-screen-connectors+
			 '((28 (4 5) (2 231) ((6 4) (7 4) (6 6) (7 6)))
			   (-28 (11 10) (2 230) ((6 11) (7 11) (6 9) (7 9)))
			   (34 (8 9) (1 1) ((5 7) (5 10)))))

(defconstant +river-out-connectors+
			 '((5 6) (18 18) ((5 4) (5 7))))

(defun internal-connect (vertex vertex-array screen biomes base)
  (let ((screen-vertices 
		  (if (listp (get-arr2d vertex-array (connect-vertex-data vertex)))
			(remove vertex (get-arr2d vertex-array (connect-vertex-data vertex))
					:test #'equal)
			nil)))
	(if screen-vertices
	  (labels ((connect-vert-split (vert-info)
				 (let ((opts (remove-if
							   (lambda (col)
								 (not (and (assoc (aref screen col)
										+vert-split-screen-replacements+)
										   (intersection
											 `((2 ,col) (8 ,col))
											 (cdddr 
											   (connect-vertex-data vertex))
											 :test #'equal)
										   (some
											 (lambda (vert)
											   (and
											     (intersection
												   `((2 ,col) (8 ,col))
												   (cdddr
												     (connect-vertex-data vert))
												   :test #'equal)
												 (not
												   (eql (connect-vertex-mark
														  vert)
														(connect-vertex-mark
														  vertex)))))
											 screen-vertices))))
							   vert-info)))
				   (if opts
					 (let ((opt (nth (mrandom (length opts)) opts)))
					   (setf (aref screen opt)
							 (cdr (assoc (aref screen opt)
										 +vert-split-screen-replacements+)))
					   (connect-vertex-mark
						 (find-if (lambda (vert)
									(intersection `((2 ,col) (8 ,col))
												  (cdddr (connect-vertex-data
														   vert))
												  :test #'equal))
								  screen-vertices)))
					 nil)))
			   (connect-horiz-split (horiz-info)
				 (let ((other
						 (remove-if (lambda (vert)
									  (not (intersection 
											 (cdddr (connect-vertex-data vert))
											 (caddr horiz-info)
											 :test #'equal)))
									screen-vertices)))
				   (if (and other
							(intersection (cdddr (connect-vertex-data vertex))
										  (caddr horiz-info) :test #'equal))
					 (progn
					   (mapc (lambda (col repl)
							   (setf (aref screen col) repl))
							 (car horiz-info)
							 (cadr horiz-info))
					   (connect-vertex-mark other))
					 nil))))
	  (let* ((base-screen (get-arr2d base (connect-vertex-data vertex))))
		(cond ((assoc base-screen +vert-split-screen-connectors+)
			   (connect-vert-split 
				 (cadr (assoc base-screen +vert-split-screen-connectors+))))
			  ((assoc base-screen +horiz-split-screen-connectors+)
			   (connect-horiz-split
				 (cdr (assoc base-screen +horiz-split-screen-connectors+))))
			  ((member 'river-out 
					   (get-arr2d biomes (connect-vertex-data vertex)))
			   (connect-horiz-split +river-out-connectors+))
			  (t nil)))))))


;connect-vertex-data specification
;In zelda-edges, connect-vertex-data only contains the coordinate in which
;the vertex resides, e.g. '(0 0)
;Here, additional traits are added to the end of the list.
;Everything after the coordinate is optional.
;'(y-coord x-coord pos-in-list tile-positions)
;y-coord: the vertical coordinate on the map; for '(0 1), it is 0. REQUIRED
;x-coord: the horizontal coordinate on the map; for '(0 1), it is 1. REQUIRED
;pos-in-list: coordinates with multiple vertices have them represented as a
;			  list in the vertex-array. The pos-in-list is which position that
;			  vertex has in its list.
;tile-positions: For screens with multiple vertices, this is a list of all
;				 tile coordinates this vertex contains.
(defun get-vertex-by-data (vertex-array data)
  (if (cddr data)
	(nth (caddr data) (get-arr2d vertex-array data))
	(get-arr2d vertex-array data)))

(defun set-vertex-by-data (vertex-array data new-vertex)
  (setf
	(if (cddr data)
	  (nth (caddr data) (get-arr2d vertex-array data))
	  (get-arr2d vertex-array data))
	new-vertex))

(defsetf get-vertex-by-data set-vertex-by-data)

(defun passable (tile)
  (member tile '(#x0A #x0E #x0B #x26)))

(defun get-tile-from-screen (screen coord)
  (nth (car coord) (column-by-number (aref screen (cadr coord)))))

(defun get-vertex-adjacent-to (vertex-array screen-coord tile-coord screen-width
											screen-height)
  (let* ((side (cond ((eql (car tile-coord) 0) 0)
					 ((eql (car tile-coord) (1- screen-height)) 1)
					 ((eql (cadr tile-coord) 0) 2)
					 ((eql (cadr tile-coord) (1- screen-width)) 3)
					 (t nil)))
		 (vertices (when side
					 (get-arr2d vertex-array
								(funcall 
								  (nth side
									   (list
										 (lambda (x) `(,(1- (car x)) ,(cadr x)))
										 (lambda (x) `(,(1+ (car x)) ,(cadr x)))
										 (lambda (x) `(,(car x) ,(1- (cadr x))))
										 (lambda (x) 
										   `(,(car x) ,(1+ (cadr x))))))
								  screen-coord)))))
	(if side
	  (if (listp vertices)
		(find-if (lambda (x)
				   (some (nth side
							  (list
								(lambda (y) (equal y `(,(1- screen-height)
													   ,(cadr tile-coord))))
								(lambda (y) (equal y `(0 ,(cadr tile-coord))))
								(lambda (y) (equal y `(,(car tile-coord)
												       ,(1- screen-width))))
								(lambda (y) (equal y `(,(car tile-coord) 0)))))
						 (nthcdr 3 (connect-vertex-data x))))
				 vertices)
		vertices)
	  nil)))

(defun simplify-screen-edges-to-vertex (edges-list priority-list)
  (mapcar (lambda (dest)
			(reduce (lambda (x y)
					  (if (< (position (connect-edge-kind x) priority-list)
							 (position (connect-edge-kind y) priority-list))
						x y))
					(remove-if (lambda (edge)
								 (not (equal (connect-edge-dest edge) dest)))
							   edges-list)))
		  (remove-duplicates (mapcar (lambda (edge) (connect-edge-dest edge))
									   edges-list) :test #'equal)))
										

(defun add-edge-if-needed (vertex edge)
  (unless (some (lambda (other) (equal (connect-edge-dest edge)
									   (connect-edge-dest other)))
				(connect-vertex-edges vertex))
	(push edge (connect-vertex-edges vertex))))


(defun simplify-screen-to-vertex (vertex-array coord new-mark)
  (let ((edge-priority '(dock bridge edge wall))
		(simplified-vertex (make-connect-vertex 
							 :data (subseq coord 0 2) :mark new-mark))
		(vertices (get-arr2d vertex-array coord)))
	(if (listp vertices)
	  (progn
		(mapc 
		  (lambda (neighbor)
			(setf (connect-vertex-edges neighbor)
				  (remove-if (lambda (edge)
							   (equal (subseq (connect-edge-dest edge) 0 2)
									  coord))
							 (connect-vertex-edges neighbor))))
		  (apply #'append
				 (mapcar (lambda (coord)
						   (if (listp (get-arr2d vertex-array coord))
							 (get-arr2d vertex-array coord)
							 `(,(get-arr2d vertex-array coord))))
						 (get-neighbor-coords
						   (array-dimension vertex-array 1)
						   (array-dimension vertex-array 0)
						   coord))))
		(let* ((all-edges 
				 (apply 
				   #'append 
				   (mapcar (lambda (vertex) (connect-vertex-edges vertex))
						   vertices)))
			   (simp-edges (simplify-screen-edges-to-vertex
							 all-edges edge-priority)))
		  (mapc (lambda (edge) 
				  (push (make-connect-edge :source coord 
										   :dest (connect-edge-dest edge)
								   :kind (connect-edge-kind edge))
						(connect-vertex-edges simplified-vertex))
				  (push (make-connect-edge :source (connect-edge-dest edge) 
										   :dest coord
								   :kind (connect-edge-kind edge))
						(connect-vertex-edges
						  (get-vertex-by-data vertex-array 
											  (connect-edge-dest edge)))))
				simp-edges))
		(set-arr2d vertex-array coord simplified-vertex))
	  (setf (connect-vertex-mark vertices) new-mark))))


(defun remove-edge-to (vertex coord)
  (setf (connect-vertex-edges vertex)
		(remove-if (lambda (x) (equal (connect-edge-dest x) coord))
				   (connect-vertex-edges vertex))))

(defun rebuild-vertex (vertex-array screens base biomes vertex coord)
  (labels ((find-adj-vertices (screen-width screen-height vertex group)
			(mapcar
			  (lambda (matcher)
				(remove-duplicates
				(mapcar (lambda (coord) 
						  (get-vertex-adjacent-to vertex-array
												  (subseq (connect-vertex-data
															vertex)
														  0 2)
												  coord
												  screen-width
												  screen-height))
						(remove-if matcher group)) :test #'equal))
			  `(,(lambda (x) (not (eql (car x) 0)))
				,(lambda (x) (not (eql (car x) (1- screen-height))))
				,(lambda (x) (not (eql (cadr x) 0)))
				,(lambda (x) (not (eql (cadr x) (1- screen-width)))))))
		   (match-edge-to-coord (edge coord)
								(equal (subseq (connect-edge-dest edge) 0 2)
									   coord))
		   (rebuild-edges (source-vertex dest-vertices)
			 (let ((source-coord (subseq (connect-vertex-data source-vertex)
										 0 2))
				   (source-access (subseq (connect-vertex-data source-vertex)
										 0 3)))
			   (mapc (lambda (vertex)
					   (let ((old-kind (connect-edge-kind
										 (find-if 
										   (lambda (edge)
											 (match-edge-to-coord edge 
																  source-coord))
										   (connect-vertex-edges vertex))))
							 (accessor (subseq (connect-vertex-data vertex)
											   0 
											   (if 
												 (> 
												   (length (connect-vertex-data
															 vertex)) 2)
												 3 2))))
						 
						 (push (make-connect-edge :source accessor
										  :dest source-access
										  :kind old-kind)
							   (connect-vertex-edges vertex))
						 (push (make-connect-edge :source source-access
										  :dest accessor
										  :kind old-kind)
							   (connect-vertex-edges source-vertex))))
					 dest-vertices)))
		   (remove-edges-to (vertex-coord)
			 (mapc (lambda (coord)
					 (let ((vertices (get-arr2d vertex-array coord)))
					   (if (listp vertices)
						 (mapc (lambda (vertex)
								 (remove-edge-to vertex vertex-coord))
							   vertices)
						 (remove-edge-to vertices vertex-coord))))
				   (remove-if (lambda (coord) (not (valid-coordp
													 (array-dimension base 1)
													 (array-dimension base 0)
													 coord)))
							  (mapcar (lambda (delta)
										`(,(+ (car delta) (car vertex-coord))
										  ,(+ (cadr delta) 
											  (cadr vertex-coord))))
									  '((-1 0) (1 0) (0 -1) (0 1))))))
		   (handle-one-vertex (vertex coord)
			 (let* ((screen (get-arr2d screens coord))
					(screen-width (length screen))
					(screen-height (length (column-by-number (aref screen 0))))
					(split (split-vertex-if-needed vertex screen)))
			   (when (listp split)
				 (set-arr2d vertex-array coord split)
				 (mapc (lambda (sub-vert)
						 (mapc (lambda (dest-set)
								 (rebuild-edges sub-vert dest-set))
							   (find-adj-vertices screen-width screen-height
												  sub-vert
												  (nthcdr 
													3 
											(connect-vertex-data sub-vert)))))
					   split)
				 (remove-edges-to coord)
				 (when (member 'river-in (get-arr2d biomes coord))
				   (let ((piece1 (subseq (connect-vertex-data (car split)) 0 3))
						 (piece2 (subseq (connect-vertex-data
										   (cadr split)) 0 3)))
					 (push (make-connect-edge :source piece1 :dest piece2 
											  :kind 'river)
						   (connect-vertex-edges (car split)))
					 (push (make-connect-edge :source piece2 :dest piece1 
											  :kind 'river)
						   (connect-vertex-edges (cadr split)))))
				 (when (some (lambda (x) (eq (connect-edge-kind x) 'dock))
							 (connect-vertex-edges vertex))
				   (let* ((dock-vertex
						   (find-if (lambda (sub-vert)
									  (some (lambda (coord)
											  (eql (car coord) 4))
											(nthcdr 
											  3 
											  (connect-vertex-data sub-vert))))
									split))
						  (dock-access
							(subseq (connect-vertex-data dock-vertex) 0 3))
						  (island-coord `(,(1- (car coord)) ,(cadr coord))))
					 (push (make-connect-edge :source dock-access
									  :dest island-coord
									  :kind 'dock)
						   (connect-vertex-edges dock-vertex))
					 (push (make-connect-edge :source island-coord
									  :dest dock-access
									  :kind 'dock)
						   (connect-vertex-edges
							 (get-arr2d vertex-array island-coord)))))))))
	(handle-one-vertex vertex coord)))

(defun rebuild-graph (vertex-array screens base biomes)
	(let ((coords 
			(remove-if (lambda (coord)
						 (not (or (member (abs (get-arr2d base coord))
										  +base-split-screens+)
								  (intersection (get-arr2d biomes coord)
												'(river-in river-out)))))
					   (get-coords-list (array-dimension base 1)
										(array-dimension base 0)))))
	  (mapc (lambda (vertex coord)
			  (rebuild-vertex vertex-array screens base biomes vertex coord))
			(mapcar (lambda (coord) (get-arr2d vertex-array coord))
					coords)
			coords))
	(let* ((left-coast-bridge (find-if (lambda (x) (eql (get-arr2d base x) 130))
									   (get-coords-list (array-dimension base 1)
														(array-dimension
														  base 0))))
		   (right-coast-bridge (if left-coast-bridge 
								 `(,(car left-coast-bridge)
								   ,(1+ (cadr left-coast-bridge)))
								 nil)))
	  (when left-coast-bridge
		(setf (connect-edge-kind
				(find-if (lambda (edge) (equal (connect-edge-dest edge)
											   right-coast-bridge))
						 (connect-vertex-edges 
						   (get-arr2d vertex-array left-coast-bridge))))
			  'river)
		(setf (connect-edge-kind
				(find-if (lambda (edge) (equal (connect-edge-dest edge)
											   left-coast-bridge))
						 (connect-vertex-edges 
						   (get-arr2d vertex-array right-coast-bridge))))
			  'river))))

(defun split-vertex-if-needed (vertex screen)
  (labels ((screen-dfs (coord markers mark)
			 (unless (get-arr2d markers coord)
			   (progn
				 (setf (aref markers (car coord) (cadr coord)) mark)
				 (mapc
				   (lambda (coord) (screen-dfs coord markers mark))
				   (remove-if
					 (lambda (coord)
					   (or (< (car coord) 0) (< (cadr coord) 0)
						   (>= (cadr coord) (length screen))
						   (>= (car coord) 
							   (length
								 (column-by-number (aref screen (cadr coord)))))
						   (not (passable 
								  (get-tile-from-screen screen coord)))))
					 (mapcar (lambda (alt) `(,(+ (car alt) (car coord))
											 ,(+ (cadr alt) (cadr coord))))
							 '((-1 0) (1 0) (0 -1) (0 1))))))))
		   (count-groups (markers)
			 (1+ (apply #'max (remove-if (lambda (x) (not x))
							 (mapcar (lambda (coord) (get-arr2d markers coord))
									 (get-coords-list
									   (array-dimension markers 1)
									   (array-dimension markers 0)))))))
		   (split-to-groups (markers groups)
			 (mapc (lambda (coord)
					 (when (get-arr2d markers coord)
					   (push coord (nth (get-arr2d markers coord) groups))))
				   (get-coords-list (array-dimension markers 1)
									(array-dimension markers 0)))
			 groups))
	(let ((markers (make-array `(,(length (column-by-number 
											(aref screen 0)))
							     ,(length screen))))
		  (current-mark -1))
	  (mapc (lambda (coord)
			  (when (and (passable (get-tile-from-screen screen coord))
						 (not (get-arr2d markers coord)))
				(screen-dfs coord markers (incf current-mark))))
			(get-coords-list (array-dimension markers 1)
							 (array-dimension markers 0)))
	  (let ((num-groups (count-groups markers)))
		(if (eql num-groups 1)
		  vertex
		  (let ((groups (split-to-groups markers (make-list num-groups))))
			(mapcar (lambda (group)
					  (make-connect-vertex
						:data (append (connect-vertex-data vertex)
									  `(,(get-arr2d markers (car group)))
									  group)))
					groups)))))))

(defun get-all-vertices (vertex-array)
  (apply #'append
		 (mapcar (lambda (coord)
				   (let ((vertices (get-arr2d vertex-array coord)))
					 (if (listp vertices) vertices `(,vertices))))
				 (get-coords-list (array-dimension vertex-array 1)
								  (array-dimension vertex-array 0)))))


(defun get-neighbor-coords (width height coord)
  (remove-if (lambda (x) (or (< (car x) 0) (>= (car x) height)
							 (< (cadr x) 0) (>= (cadr x) width)))
			 (mapcar (lambda (x) `(,(+ (car coord) (car x))
								   ,(+ (cadr coord) (cadr x))))
					 '((-1 0) (1 0) (0 -1) (0 1)))))

(defun get-columns-represented (vertex)
  (if (cddr (connect-vertex-data vertex))
	(remove-duplicates (mapcar #'cadr (nthcdr 3 (connect-vertex-data vertex))))
	(interval 2 13)))

(defun find-furthest-open-tile-in-column (column vert)
  (find-if (lambda (y)
			 (passable (nth y column)))
		   (apply #'interval (if (eq vert 'top) `(0 ,(length column))
							   `(,(length column) 0)))))

(defun get-vertex-by-tile (vertex-array coord tile-coord)
  (if (listp (get-arr2d vertex-array coord))
	(find-if (lambda (vertex)
			   (member tile-coord (cdddr (connect-vertex-data vertex)) 
					   :test #'equal))
			 (get-arr2d vertex-array coord))
	(get-arr2d vertex-array coord)))


(defun pick-open-tile (tile-coords screen-coord screens)
  (find-if (lambda (coord) 
			 (passable (get-tile-from-screen (get-arr2d screens screen-coord)
											 coord)))
		   tile-coords))


(defun reconnect-part-screens (screens base biomes vertex-array connected-sets
									   horizontal-edges vertical-edges)
  (labels ((remove-vertex (vertex)
			 (set-vertex-by-data vertex-array (connect-vertex-data vertex)
								 (make-connect-vertex 
								   :data (subseq (connect-vertex-data vertex)
												 0 3)
								   :mark 'no-vertex)))
		   (erase-set (change-set)
					  (let ((screen (get-arr2d screens (connect-vertex-data
														 (car change-set)))))
					  (if (and (eql (length change-set) 1)
							   (notany (lambda (x)
										    (notany 
											  (lambda (y)
											    (member
											    (get-tile-from-screen
												  screen
												  y) 
											    '(#x26 #x29 #x2c #x2a #x2b
												  #x0c #x28 #x27)))
											  (get-neighbor-coords
											    (length screen)
											    (length (column-by-number 0))
											    x)))
										  (nthcdr 3 (connect-vertex-data
													  (car change-set)))))
						(if (member (get-arr2d base (connect-vertex-data
													  (car change-set)))
									'(88 93 103 104 109))
						  (if (notany (lambda (x)
										 (or (eql (car x) 2)
											 (member (aref screen (cadr x))
											   '(87 116))))
									   (nthcdr 3 (connect-vertex-data
												   (car change-set))))
							(prog1 (mapcar (lambda (x)
									  (setf (aref screen x)
											(if (eql (aref screen x) 48)
											  41 21)))
									(get-columns-represented (car change-set)))
							  (remove-vertex (car change-set)))
							nil)
						  (prog1 (mapcar (lambda (x)
									(setf (aref screen x)
										  (if (member
												'(forest woods)
												(get-arr2d biomes
														   (connect-vertex-data
															 (car change-set))))
											21 0)))
										 (get-columns-represented
										   (car change-set)))
							(remove-vertex (car change-set)))) nil)))
		   (filter-external (source other extra-cond adapt-fun edge)
				  (or (not (member edge '(edge wall)))
				      (some (lambda (coord)
							  (or (numberp (get-arr2d screens coord))
								  (intersection (get-arr2d biomes coord)
												'(a-fairy open-ruin island 
												  spring pond river-in))))
						    `(,source ,other))
					  (not (funcall extra-cond other))))
		   (adapt-vert-external (source other source-side)
			 (let* ((vert-opts (vert-adaptable-cols source other source-side))
					(choice (nth (mrandom (length vert-opts)) vert-opts))
					(mark (connect-vertex-mark
							(get-vertex-by-tile 
							  vertex-array other
							  `(,(find-furthest-open-tile-in-column
								   (column-by-number
								     (aref (get-arr2d screens other) choice))
								   (if (eq source-side 'top) 'bot 'top))
								,choice))))
					(top (if (eq source-side 'top) other source))
					(bot (if (eq source-side 'top) source other))
					(edges1 (find-vert-exits (get-arr2d screens top) 'bot))
					(edges2 (find-vert-exits (get-arr2d screens bot) 'top))
					(edge-type (vert-adapt-type
								 (get-arr2d screens top)
								 (get-arr2d base top)
								 edges1
								 (get-arr2d screens bot)
								 (get-arr2d base bot)
								 edges2))
					(adder (if (eq edge-type 'open) 
							 #'add-vert-open #'add-ladder)))
			   (mapc (lambda (x vert)
					   (funcall adder (get-arr2d screens x)
								(get-arr2d base x) choice vert))
					 `(,top ,bot) '(bot top))
			   (setf (aref vertical-edges (car top) (cadr top)) 'edge)
			   mark))
		   (vert-adaptable-cols (source other source-side)
			 (let ((source-cols
					 (remove-if (lambda (x)
								  (not 
									(member 
									  `(,(find-furthest-open-tile-in-column
										   (column-by-number
											 (aref 
											   (get-arr2d screens source) x))
										   source-side) ,x)
									  (nthcdr 3 source)
									  :test #'equal)))
								(get-columns-represented
								  (get-vertex-by-data vertex-array source))))
				   (other-cols
					 (if (listp (get-arr2d vertex-array other))
					   (remove-if (lambda (x)
									(or 
									  (not 
										(find-furthest-open-tile-in-column
										  (column-by-number
											(aref
											  (get-arr2d screens other) x))
										  (if (eq source-side 'top)
											'bot 'top)))
									(eql (connect-vertex-mark
										   (get-vertex-by-data vertex-array
															   source))
										 (connect-vertex-mark
										   (find-if
											 (lambda (ov)
											   (member
										`(,(find-furthest-open-tile-in-column
											 (column-by-number
											   (aref
												 (get-arr2d screens other) x))
											 (if (eq source-side 'top)
											   'bot 'top)) ,x)
										(nthcdr 3 (connect-vertex-data ov))
										:test #'equal))
											 (get-arr2d vertex-array other))))))
								  (interval 2 13))
					   (if (eql (connect-vertex-mark 
								  (get-arr2d vertex-array other))
								(connect-vertex-mark 
								  (get-vertex-by-data vertex-array source)))
						 nil (interval 2 13)))))
			   (remove-if (lambda (x)
							(not (and (member x source-cols)
									  (member x other-cols)
									  (edges-valid-p
										(get-arr2d screens source)
										`(,x) *cols-not-to-replace*)
									  (edges-valid-p
										(get-arr2d screens other)
										`(,x) *cols-not-to-replace*))))
						  (interval 2 13))))
		   (sidemost (vertex-data dir limit)
			(if (cddr vertex-data)
			 (let ((sidemost
					 (reduce (lambda (x y)
							   (if (funcall dir (cadr x) (cadr y)) x y))
									 (cdddr vertex-data))))
			   (or (funcall limit sidemost)
				   (every (lambda (column)
							(member column '(0 21 n-rock-water-wall
											 s-rock-water-wall
											 n-tree-water-wall
											 s-tree-water-wall)))
						  (mapcar (lambda (x) (aref (get-arr2d screens 
															   vertex-data) x))
								  (remove-if 
									(lambda (x)
									  (not (funcall dir x (cadr sidemost))))
									(interval 0 15))))))
			 t))
		   (has-horiz (source other source-side)
			 (and (sidemost source (if (eq source-side 'left) #'< #'>)
							(if (eq source-side 'left)
							  (lambda (sidemost) (< (cadr sidemost) 3))
							  (lambda (sidemost) (> (cadr sidemost) 12))))
				  (if (listp (get-arr2d vertex-array other))
					(some (lambda (vertex)
							(and (sidemost (connect-vertex-data vertex)
										   (if (eq source-side 'left)
											 #'> #'<)
										   (if (eq source-side 'left)
											 (lambda (sidemost) 
											   (> (cadr sidemost) 12))
											 (lambda (sidemost) 
											   (< (cadr sidemost) 3))))
								 (not (eql (connect-vertex-mark vertex)
										   (connect-vertex-mark
											 (get-vertex-by-data 
											   vertex-array source))))))
						  (get-arr2d vertex-array other))
					(not (eql (connect-vertex-mark
								(get-arr2d vertex-array other))
							  (connect-vertex-mark
								(get-vertex-by-data vertex-array source)))))))
					  
		   (adapt-horiz-external (source other source-side)
			 (let ((left (if (eq source-side 'left) other source))
				   (right (if (eq source-side 'left) source other)))
			   (preserve-vert-adapt-side
				 (get-all-adapted left) (get-all-adapted right)
				 (get-arr2d base left) (get-arr2d base right)
				 (get-arr2d screens left) (get-arr2d screens right)
				 t *extends*)
			   (setf (aref horizontal-edges (car left) (cadr left)) 'edge))
			 (connect-vertex-mark
			   (if (listp (get-arr2d vertex-array other))
				 (find-if (lambda (vertex)
							(and (sidemost (connect-vertex-data vertex)
										   (if (eq source-side 'left)
											 #'> #'<)
										   (if (eq source-side 'left)
											 (lambda (sidemost) 
											   (> (cadr sidemost) 12))
											 (lambda (sidemost)
										       (< (cadr sidemost) 3))))
								 (not (eql (connect-vertex-mark vertex)
										   (connect-vertex-mark 
											 (get-vertex-by-data vertex-array
																 source))))))
						  (get-arr2d vertex-array other))
				 (get-arr2d vertex-array other))))
		   (connect-external (vertex-sets)
			 (let ((opts
					 (remove-if 
					   (lambda (opt) (apply #'filter-external opt))
					   (apply
						 #'append
						 (mapcar 
						   (lambda (vertex)
							 (mapcar (lambda (other extra-cond adapt-fun edge)
									   `(,(connect-vertex-data vertex)
										 ,other ,extra-cond ,adapt-fun
										 ,edge))
									 (mapcar (lambda (alt)
											   `(,(+ (car alt) 
													 (car (connect-vertex-data
															vertex)))
											     ,(+ (cadr alt) 
													 (cadr (connect-vertex-data
															 vertex)))))
											 '((-1 0) (1 0) (0 -1) (0 1)))
									 `(,(lambda (other)
										  (vert-adaptable-cols
											(connect-vertex-data vertex)
											other 'top))
									   ,(lambda (other)
										  (vert-adaptable-cols
											(connect-vertex-data vertex)
											other 'bot))
									   ,(lambda (other)
										  (has-horiz
											(connect-vertex-data vertex)
											other 'left))
									   ,(lambda (other)
										  (has-horiz
											(connect-vertex-data vertex)
											other 'right)))
									 `(,(lambda (other)
										  (adapt-vert-external
											(connect-vertex-data vertex)
											other 'top))
									   ,(lambda (other)
										  (adapt-vert-external
											(connect-vertex-data vertex)
											other 'bot))
									   ,(lambda (other)
										  (adapt-horiz-external
											(connect-vertex-data vertex)
											other 'left))
									   ,(lambda (other)
										  (adapt-horiz-external
											(connect-vertex-data vertex)
											other 'right)))
									 `(,(north-edge 
										  horizontal-edges vertical-edges
										  (car (connect-vertex-data vertex))
										  (cadr (connect-vertex-data vertex)))
									   ,(south-edge 
										  horizontal-edges vertical-edges
										  (car (connect-vertex-data vertex))
										  (cadr (connect-vertex-data vertex)))
									   ,(west-edge 
										  horizontal-edges vertical-edges
										  (car (connect-vertex-data vertex))
										  (cadr (connect-vertex-data vertex)))
									   ,(east-edge 
										  horizontal-edges vertical-edges
										  (car (connect-vertex-data vertex))
										  (cadr 
											(connect-vertex-data vertex))))))
						   (cdar (last vertex-sets)))))))
			   (if opts
				 (let ((opt (nth (mrandom (length opts)) opts)))
				   (repair-external vertex-sets (car opt) (cadr opt)
									(funcall (cadddr opt) (cadr opt))))
				 nil)))
		   (replace-all-marks (vertex-sets old-mark new-mark)
			 (let ((change-set (cdr (assoc old-mark vertex-sets
										   :test #'eql))))
			   (mapc (lambda (vertex)
					   (setf (connect-vertex-mark vertex) new-mark))
					 change-set)
			   (setf (cdr (assoc new-mark vertex-sets))
					 (append (cdr (assoc new-mark vertex-sets))
							 change-set))
			   (remove-if (lambda (x) (eql (car x) old-mark))
						  vertex-sets)))
		   (repair-external (vertex-sets source other new-mark)
			 (let ((source-vertices (get-arr2d vertex-array source))
				   (other-vertices (get-arr2d vertex-array other)))
			   (format t "new: ~a; old: ~a" new-mark
					   (connect-vertex-mark (get-vertex-by-data vertex-array
																source)))
			   (setf vertex-sets 
					 (replace-all-marks 
					   vertex-sets
					   (connect-vertex-mark (get-vertex-by-data vertex-array
																source))
					   new-mark))
			   (when (listp source-vertices)
				 (simplify-screen-to-vertex vertex-array source nil))
			   (when (listp other-vertices)
				 (simplify-screen-to-vertex vertex-array other nil))
			   (add-edge-if-needed
				 (get-arr2d vertex-array source)
				 (make-connect-edge :source (subseq source 0 2) 
									:dest (subseq other 0 2)
									:kind 'edge))
			   (add-edge-if-needed
				 (get-arr2d vertex-array other)
				 (make-connect-edge :source (subseq other 0 2) 
									:dest (subseq source 0 2)
									:kind 'edge))
			   (when (listp source-vertices)
				 (rebuild-vertex vertex-array screens base biomes
								 (get-arr2d vertex-array source) 
								 (subseq source 0 2))
				 (setf vertex-sets
					   (choose-marks (get-arr2d vertex-array source)
									 source-vertices 
									 (if (listp other-vertices) 
									   other-vertices nil)
									 vertex-sets)))
			   (when (listp other-vertices)
				 (rebuild-vertex vertex-array screens base biomes
								 (get-arr2d vertex-array other) 
								 (subseq other 0 2))
				 (setf vertex-sets
					   (choose-marks (get-arr2d vertex-array other)
									 other-vertices
									 (if (listp source-vertices)
									   source-vertices nil)
									 vertex-sets))))
			 vertex-sets)
		   (choose-marks (new-vertices old-vertices other-olds vertex-sets)
			 (mapc (lambda (new-v)
					 (let ((marks
							 (remove-duplicates
							   (mapcar (lambda (old-v)
										 (connect-vertex-mark old-v))
									   (remove-if
										 (lambda (old-v)
										   (not
											 (member
											   (pick-open-tile
												 (cdddr (connect-vertex-data
														 old-v))
												 (subseq (connect-vertex-data
														   old-v) 0 2)
												 screens)
											   (cdddr (connect-vertex-data
													   new-v))
											   :test #'equal)))
										 old-vertices))
							   :test #'eql)))
					   (if (> (length marks) 1)
						 (let ((mark-choice (find-if
											  (lambda (mark) 
												(member mark marks))
											  (mapcar #'car vertex-sets))))
						   (mapc (lambda (old-mark)
								   (setf vertex-sets (replace-all-marks
													   vertex-sets
													   old-mark
													   mark-choice)))
								 (remove mark-choice marks :test #'eql))
						   (mapc (lambda (old-v)
								   (when (member (connect-vertex-mark old-v)
												 marks :test #'eql)
									 (setf (connect-vertex-mark old-v)
										   mark-choice)))
								 (append old-vertices other-olds))
						   (setf (connect-vertex-mark new-v) mark-choice)
						   (push new-v (cdr (assoc mark-choice vertex-sets))))
						 (progn (setf (connect-vertex-mark new-v) (car marks))
								(push new-v 
									  (cdr (assoc (car marks) vertex-sets)))))))
				   (if (listp new-vertices) new-vertices `(,new-vertices)))
			 vertex-sets)
		   (connect-internal (vertex-sets)
			 (let ((new-mark nil)
				   (change-vert 
					 (find-if (lambda (vert)
								(setf new-mark
									  (internal-connect vert vertex-array
														(get-arr2d 
														  screens
													(connect-vertex-data vert))
													biomes base)))
							  (cdar (last vertex-sets)))))
			   (if change-vert
				 (progn
				   (setf vertex-sets
						 (replace-all-marks vertex-sets
											(connect-vertex-mark change-vert)
											new-mark))
				   (let ((olds (get-arr2d vertex-array
										  (connect-vertex-data change-vert))))
					 (simplify-screen-to-vertex vertex-array
											  (connect-vertex-data change-vert)
												nil)
					 (rebuild-vertex vertex-array screens base biomes
									 change-vert
									 (connect-vertex-data change-vert))
					 (setf vertex-sets
						   (choose-marks (get-arr2d vertex-array
													(subseq 
													  (connect-vertex-data
													   change-vert) 0 2))
										 olds nil vertex-sets))
					 vertex-sets))
				 nil)))
		   (handle-one-set (vertex-sets)
			 (unless (eql (length vertex-sets) 1)
			   (if (erase-set (cdar (last vertex-sets)))
				 (handle-one-set (butlast vertex-sets))
				 (let ((ext-result (connect-external vertex-sets)))
				   (if ext-result
					 (handle-one-set ext-result)
					 (let ((int-result (connect-internal vertex-sets)))
					   (if int-result
						 (handle-one-set int-result)
						 (progn
						   (princ 
						"Error connecting partial screens; no options found")
						   (error 
							 'invalid-random-maximum-error :maximum 0))))))))))
	(handle-one-set
	  (mapcar (lambda (con-set)
				(append `(,(connect-vertex-mark (car con-set))) con-set))
			  connected-sets))))

(defun final-connect-map (screens base biomes vertex-array horizontal-edges
								  vertical-edges)
  (mapc (lambda (coord)
		  (setf (connect-vertex-mark (get-arr2d vertex-array coord)) nil))
		(get-coords-list (array-dimension base 1) (array-dimension base 0)))
  (rebuild-graph vertex-array screens base biomes)
  (mapc (lambda (vertex marker)
		  (connect-dfs vertex (lambda (x) nil) (lambda (x) nil) marker
					   (lambda (vertex)
						 (mapcar (lambda (edge)
								   (get-vertex-by-data vertex-array
													(connect-edge-dest edge)))
								   (connect-vertex-edges vertex)))))
		  (get-all-vertices vertex-array)
		  (interval 0 (1- (length (get-all-vertices vertex-array)))))
  (reconnect-part-screens screens base biomes vertex-array
						  (separate-to-connected-sets 
							(get-all-vertices vertex-array))
						  horizontal-edges vertical-edges))
