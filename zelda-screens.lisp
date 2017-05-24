;;zelda-screens.lisp
;;Part of the zelda overworld randomizer
;;This particular piece handles putting relevant screens onto the map,
;;including duplicate screens. It does not do the work to fit them together,
;;and it is dependent on the biomes already being placed, the columns
;;being adapted, and the edges being decided already.
;;@author Nathaniel Schleicher

(load "normal-biome-exits")
(defparameter *new-screens* (make-array '(8 16)))
(defparameter *new-base-screens* (make-array '(8 16)))
(defparameter *new-rom-screens* (make-array 124))
(defparameter *new-map* nil)

;Edges-adapted in order north south west east, as (collect-edges)
(defparameter *edges-adapted* (make-array '(8 16)))
(defparameter *spec-screens-lists* nil)
(defparameter *normal-screens-list* nil)

(defun invert-column (column)
  (if (numberp column)
	(opp-column-by-number column)
	(let* ((name-opp1 (cdr (assoc column +opposite-names+)))
		   (name-opp2 (car (rassoc column +opposite-names+))))
	  (if name-opp1 name-opp1
		(if name-opp2 name-opp2
		  column)))))

(defun invert-screen (screen)
  (reverse (mapcar #'invert-column screen)))

(defparameter *name-screens*
  (coerce (append (mapcar
			(lambda (x)
			  (mapcar 
				(lambda (y) (let ((name (car (rassoc y +init-column-names+))))
							  (if name name y)))
				x))
			(coerce *screens* 'list))
				  '((54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54)
					(66 66 66 66 66 66 66 66 97 97 97 97 97 97 97 97)
					(66 66 66 66 66 66 66 88 246 br-water water water water 
					 water water water)
					(water water water water water water water 180
						   176 176 115 115 115 115 115 115)
					(165 164 200 144 8 8 8 8 8 river-bend 115 115 
					 115 115 115 115)
					(167 166 1 129 167 241 169 bl-rock 54 54 54 54 82 82 54 54)
					(151 145 81 81 151 88 88 88 246 br-water 18 water 18 
					 water 18 18)
					(18 18 water 18 water 18 bl-water 88 88 88 71 71 71 133 
					 145 81)
					(0 167 241 169 2 16 2 8 8 s-rock-water-wall 
					 s-rock-water-wall s-rock-water-wall s-rock-water-wall 
					 s-rock-water-wall s-rock-water-wall s-rock-water-wall) 
					(0 241 169 16 200 199 160 4 4 n-rock-water-wall 
					 n-rock-water-wall n-rock-water-wall n-rock-water-wall 
					 n-rock-water-wall n-rock-water-wall n-rock-water-wall) 
					(2 2 7 130 7 7 130 7 7 130 7 7 130 7 2 2)
					(2 2 8 147 8 8 147 8 8 147 8 8 147 8 2 2)
					(0 0 197 197 197 197 197 197 197 197 197 197 197 197 0 0)))
		  'array))

;Below: removing river-out from screens it is in.
(mapcar (lambda (x) (setf (nth x (aref *name-screens* 81)) 82))
		(interval 4 7))

(mapcar (lambda (x) (setf (nth x (aref *name-screens* 96)) 24))
		(interval 5 6))

(mapcar (lambda (x) (setf (nth x (aref *name-screens* 112)) 8))
		(interval 5 6))

(defun name-to-num (column)
  (if (numberp column) column
	(let ((sub (cdr (assoc column *column-names*))))
	  (if (numberp sub) sub
		(name-to-num sub)))))

(defconstant +init-duplicate-screens+ 
			 (coerce `(,(aref *name-screens* 6) ,(aref *name-screens* 10) 
					   ,(aref *name-screens* 56) ,(aref *name-screens* 75)
					   ,(invert-screen (aref *name-screens* 75))
					   ,(aref *name-screens* 111)
					   ,(invert-screen (aref *name-screens* 111))
					   ,(aref *name-screens* 118) 
					   (54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54)
					   ,(aref *name-screens* 123)) 'array))
;0: mountain E/W; 6
;1: ladder ruin; 10
;2: fairy/pond; 56
;3: E coast N/S HC possibility; 75
;4: W coast N/S HC possibility (above reversed); -75
;5: W-open ruin; 111
;6: E-open ruin (above reversed); -111
;7: flat S-coast w/ bomb spot; 118
;8: flat N-coast (no bomb spot since impossible); 124
;9: alternative fairy/pond (for use with ladders); 123

(defconstant +mountain-screens+ '(0 1 2 3 4 5 6 7 8 11 12 15 16 17 18 19 20 21 
								  26 27 34 35 36 43 49 50 76 107 108 116))

(defconstant +hill-screens+ '(70 72 90 95 96 97 98 112 113 114))

(defconstant +woods-screens+ '(77 78 79 91 92 93 94 109 110))

(defconstant +forest-screens+ '(59 69 71 73 74 83 84 87 88 89 99 102 103 104 
								105 115))

(defparameter *mountain-screens* (copy-seq +mountain-screens+))
(defparameter *hill-screens* (copy-seq +hill-screens+))
(defparameter *woods-screens* (copy-seq +woods-screens+))
(defparameter *forest-screens* (copy-seq +forest-screens+))

(defun init-screen-maker (width height)
  (setf *new-screens* (make-array (list height width)))
  (setf *new-base-screens* (make-array (list height width)))
  (setf *new-rom-screens* (make-array 124))
  (setf *new-map* nil)
  (setf *edges-adapted* (make-array (list height width)))
  (setf *mountain-screens* (copy-seq +mountain-screens+))
  (setf *hill-screens* (copy-seq +hill-screens+))
  (setf *woods-screens* (copy-seq +woods-screens+))
  (setf *forest-screens* (copy-seq +forest-screens+))
  (setf *spec-screens-lists* nil)
  (setf *normal-screens-list* nil)
  (mapc (lambda (y) (mapc 
					  (lambda (x) 
						(setf (aref *edges-adapted* y x) 
							  (copy-seq '(nil nil nil nil))))
					  (interval 0 (1- width))))
		(interval 0 (1- height))))

(defun set-new-screen (coord contents base)
  (setf (aref *new-screens* (car coord) (cadr coord)) 
		(if (numberp contents) contents
		  (coerce (copy-seq contents) 'array)))
  (setf (aref *new-base-screens* (car coord) (cadr coord)) base))

(defun set-inverse-screen (coord contents base)
  (set-new-screen coord (invert-screen contents) (- base)))

(defconstant +start-screen-lists+
			 '((open-ruin) (a-fairy) (pond) (spring) (island) (coast) 
						   (graveyard) (desert) (big-lake) (river-in) 
						   (small-lake) (river-out)))

(defun place-screens (width height biomes spec-biome-info horiz-edges
							vert-edges)
  (init-screen-maker width height)
  (let* ((screen-lists (deep-copy-list +start-screen-lists+))
		 (screen-types (mapcar #'car screen-lists))
		 (remaining nil))
	(mapc (lambda (y)
			(mapc (lambda (x)
					(let ((main-type 
							(find-if 
							  (lambda (z) (member z (aref biomes y x)))
							  screen-types)))
					  (if main-type
						(push `(,y ,x) (cdr (assoc main-type screen-lists)))
						(push `(,y ,x) remaining))
					  (when (and (member 'river-out (aref biomes y x))
								 (not (eq 'river-out main-type)))
						(push `(,y ,x) (cdr (assoc 'river-out screen-lists))))))
				  (interval 0 (1- width))))
		  (interval 0 (1- height)))
	(init-screen-maker width height)
	(setf *spec-screens-lists* screen-lists)
	(setf *normal-screens-list* remaining)
	(ruins-screens (cdr (assoc 'open-ruin screen-lists)) horiz-edges vert-edges)
	(fairy-screens (cdr (assoc 'a-fairy screen-lists)))
	(pond-screens (cdr (assoc 'pond screen-lists)))
	(spring-screens (cdr (assoc 'spring screen-lists))
					(cadr (assoc 'river-in spec-biome-info)))
	(island-screens (cdr (assoc 'island screen-lists)) horiz-edges vert-edges
					biomes)
	(both-lake-screens (cdr (assoc 'big-lake screen-lists)) horiz-edges
					  vert-edges biomes 
					  (cdr (assoc 'big-lake spec-biome-info))
					  (cadr (assoc 'small-lake spec-biome-info)))
	(river-in-screens (cdr (assoc 'river-in screen-lists)) 
					  (cadr (assoc 'river-in spec-biome-info)))
	(coast-screens (cdr (assoc 'coast screen-lists))
				   (cdr (assoc 'coast-cluster spec-biome-info))
				   horiz-edges vert-edges width height biomes)
	(grave-screens (cdr (assoc 'graveyard screen-lists)) 
				   (cdr (assoc 'grave spec-biome-info)))
	(desert-screens (cdr (assoc 'desert screen-lists)) 
					(cdr (assoc 'desert spec-biome-info)))
	(river-out-screens (cdr (assoc 'river-out screen-lists))
					   biomes
					   (cdr (assoc 'river-out spec-biome-info)))
	(duplicate-screens biomes horiz-edges vert-edges width height)
	(fill-remaining-screens remaining biomes)
	(princ *new-screens*) (fresh-line)))

(defun ruins-screens (ruins-list horiz vert)
  (mapc (lambda (x)
		  (if (eq 'edge (south-edge horiz vert (car x) (cadr x)))
			(set-new-screen x 1 10)
			(if (eq 'edge (west-edge horiz vert (car x) (cadr x)))
			  (set-new-screen x 5 111)
			  (set-new-screen x 6 -111))))
		ruins-list))

(defun set-all-screens-to-same (screen-list content base)
  (mapc (lambda (x)
		  (set-new-screen x content base))
		screen-list))

(defun fairy-screens (fairy-list)
  (set-all-screens-to-same fairy-list 2 56))

(defun pond-screens (pond-list)
  (set-all-screens-to-same pond-list 2 56))

(defun spring-screens (spring-list river-in-ids)
  (if (eq 'left (car river-in-ids))
	(set-all-screens-to-same spring-list (invert-screen (aref *name-screens* 9))
							 -9)
	(set-all-screens-to-same spring-list (aref *name-screens* 9) 9)))

(defun island-screens (island-list horiz vert biomes)
  (mapc (lambda (x)
		  (cond
			((eq (west-edge horiz vert (car x) (cadr x)) 'bridge)
				(set-new-screen x
					  (invert-screen (aref *name-screens* 54)) -54))
			((eq (east-edge horiz vert (car x) (cadr x)) 'bridge)
				(set-new-screen x
					  (aref *name-screens* 54) 54))
			((member 'big-lake (aref biomes (car x) (cadr x)))
			 (if (eq (west-edge horiz vert (1+ (car x)) (cadr x)) 'water-wall)
			   (set-inverse-screen x (aref *name-screens* 65) 65)
			   (set-new-screen x (aref *name-screens* 65) 65)))
			(t (if (member (east-edge horiz vert (1+ (car x)) (cadr x))
						   '(water-wall none))
				 (set-new-screen x
					   (aref *name-screens* 46) 46)
				 (set-new-screen x
					   (invert-screen (aref *name-screens* 46)) -46)))))
		island-list))

(defun mass-invert-screen (screen-num-list)
  (mapcar (lambda (x) (cons (invert-screen (car x)) (- (cdr x))))
		  (mass-get-screen screen-num-list)))

(defun mass-get-screen (screen-num-list)
  (mapcar (lambda (x) (cons (aref *name-screens* x) x)) screen-num-list))


;The below screens are in the following order: tl tr bl br t l r b
;Within each order are two lists: high-priority and low-priority.
;The low-priority ones only get used if the high-priority ones are exhausted.
;Each list has several screen-content + base-screen comboes. Each combo
;is a dotted list in the form (content . base-screen)

(defconstant +forest-lake-screens+
			 `((((,(aref *name-screens* 85) . 85)) ;tl
			    (((23 118 37 23 23 118 118 118 118 tl-tree 66 66 66 66 66 66)
				  . -86)
				 ((23 40 49 23 38 23 23 118 118 tl-tree 66 66 66 66 66 66)
				  . -39)))
			   (,(mass-get-screen '(39 86)) ;tr
				(((97 97 97 97 97 97 tr-tree 24 24 22 22 22 22 22 22 22)
				  . -85)))
			   (,(mass-get-screen '(80 100)) ;bl
				,(mass-invert-screen '(68 82 101)))
			   (,(mass-get-screen '(68 82 101)) ;br
				,(mass-invert-screen '(80 100)))
			   ((((66 66 66 66 66 66 66 66 97 97 97 97 97 97 97 97) . 125) ;t
				 ((97 97 97 97 97 97 97 97 66 66 66 66 66 66 66 66) 
				  . -125)) nil)
			   ((((21 23 23 37 23 40 23 24 24 l-coast water water water
				  water water water) . -55)) nil) ;l
			   ((((water water water water water water r-coast 24 24 23 40 23
						37 23 23 21) . 55)) nil) ;r
			   (((,(aref *name-screens* 67) . 67)) nil))) ;b
			   

(defconstant +forest-lake-internals+
			 `(((66 66 66 66 66 66 66 88 246 br-water water water water water 
				   water water) . 126) ;tl
			   ((water water water water water water bl-water 195 88 66 66 66 
					   66 66 66 66) . -126) ;tr
			   (,(invert-screen (aref *name-screens* 66)) . -66) ;bl
			   (,(aref *name-screens* 66) . 66))) ;br

(defconstant +rock-lake-screens+
			 `((,(mass-get-screen '(37 51)) ,(mass-get-screen '(28))) ;tl
			   (,(mass-invert-screen '(37 51)) ,(mass-invert-screen '(28))) ;tr
			   ((((6 6 6 161 2 210 162 24 24 bl-rock 54 54 54 54 54 54) . 44))
				,(mass-get-screen '(60))) ;bl
			   ((((54 54 54 54 54 54 br-rock 24 24 163 210 2 160 131 6 6) 
				  . -44))
				,(mass-invert-screen '(60))) ;br
			   (,(append (mass-get-screen '(52))
					'(((145 81 151 145 81 151 88 88 88 88 88 145 81 151 133 71) 
					  . 38)))
				,(mass-get-screen '(118 119))) ;t
			   (,(mass-get-screen '(64 106))
				,(mass-get-screen '(75))) ;l
			   (,(mass-invert-screen '(64 106))
				,(mass-invert-screen '(75))) ;r
			   ((((55 55 82 82 82 55 55 82 82 82 54 82 54 82 55 55) . 81))
				nil))) ;b

(defconstant +rock-lake-internals+
			 `((,(aref *name-screens* 53) . 53)
			   (,(invert-screen (aref *name-screens* 53)) . -53)
			   (,(aref *name-screens* 45) . 45)
			   (,(invert-screen (aref *name-screens* 45)) . -45)))

(defconstant +coast-screens+ 
			 `((,(mass-get-screen '(28)) ,(mass-get-screen '(37 51))) ;tl
			   (,(mass-invert-screen '(28)) ,(mass-invert-screen '(37 51))) ;tr
			   ((((6 6 6 161 2 210 162 24 24 bl-rock 54 54 54 54 54 54) . 44))
				,(mass-get-screen '(60))) ;bl
			   ((((54 54 54 54 54 54 br-rock 24 24 163 210 2 160 131 6 6) 
				  . -44))
				,(mass-invert-screen '(60))) ;br
			   (,(mass-get-screen '(118 119))
				,(append (mass-get-screen '(52))
					'(((145 81 151 145 81 151 88 88 88 88 88 145 81 151 133 71) 
					  . 38)))) ;t
			   (,(mass-get-screen '(75 106)) 
				,(mass-get-screen '(64))) ;l
			   (,(mass-invert-screen '(75 106)) 
				,(mass-invert-screen '(64))) ;r
			   ((((55 55 82 82 82 55 55 82 82 82 54 82 54 82 55 55) . 81))
				(((54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54) 
				  . 124))))) ;b

(defconstant +coast-internals+
			 `((,(aref *name-screens* 120) . 120)
			   (,(invert-screen (aref *name-screens* 120)) . -120)
			   (,(aref *name-screens* 45) . 45)
			   (,(invert-screen (aref *name-screens* 45)) . -45)))

;Function for easily copying out relevant sublists from above water screens.
;Arguments:
;	biome: the relevant constant list (+forest-lake-screens+, 
;	+rock-lake-screens+, or +coast-screens+)
;	side-num: the number of the side desired (0-7, meaning tl, tr, bl, br, t, l,
;	r, or b in that order)
;Output:
;	A list, copied from the input biome list, containing two elements;
;	the first is the high-priority screens for that biome, the second
;	is the low-priority screens.
(defun copy-water-screen-list (biome side-num)
  (let ((both-together (nth side-num biome)))
	(list (copy-seq (first both-together)) (copy-seq (second both-together)))))

(defun handle-water-list (pos-lst biome side-num)
  (let ((water-screen-lists (nth side-num biome)))
	(labels ((handle-r (pos-lst high-priority low-priority)
					   (when pos-lst
						 (if (or high-priority low-priority)
						   (let* ((source (if high-priority high-priority 
											low-priority))
								  (pos (mrandom (length source)))
								  (filler (nth pos source)))
							 (set-new-screen (car pos-lst)
											 (car filler)
											 (cdr filler))
							 (handle-r 
							   (cdr pos-lst)
							   (if high-priority
								 (append (subseq high-priority 0 pos)
										 (subseq high-priority (1+ pos)))
								 high-priority)
							   (if high-priority
								 low-priority
								 (append (subseq low-priority 0 pos)
										 (subseq low-priority (1+ pos))))))
						   (handle-r pos-lst (copy-seq (car water-screen-lists))
									 (copy-seq (cadr water-screen-lists)))))))
	  (handle-r pos-lst nil nil))))
					   

(defun both-lake-screens (big-lake-list horiz vert biomes lake-info
										small-lake-origin)
  (flet ((make-absolute (offsets)
						(mapcar (lambda (x)
								  `(,(+ (caar lake-info) (car x))
									,(+ (cadar lake-info) (cadr x))))
								offsets)))
	(let ((base-offs (nth (cadr lake-info)
						  '((((0 2) (1 0)) ((0 4)) ((3 0)) ((2 4) (3 2))
							 ((0 3) (1 1)) ((2 0)) ((1 4)) ((2 3) (3 1)))
						    (((0 0)) ((0 2) (1 4)) ((2 0) (3 2)) ((3 4))
							 ((0 1) (1 3)) ((1 0)) ((2 4)) ((2 1) (3 3)))
						    (((0 1) (2 0)) ((0 3)) ((4 0)) ((2 3) (4 2))
							 ((0 2)) ((1 1) (3 0)) ((1 3) (3 2)) ((4 1)))
						    (((0 0)) ((0 2) (2 3)) ((2 0) (4 1)) ((4 3))
							 ((0 1)) ((1 0) (3 1)) ((1 2) (3 3)) ((4 2))))))
		  (int-offs (nth (cadr lake-info)
						 '(((1 2) nil nil (2 2))
						   (nil (1 2) (2 2) nil)
						   ((2 1) nil nil (2 2))
						   (nil (2 2) (2 1) nil))))
		  (small-coords (append (mapcar (lambda (x)
								  `(,(+ (car x) (car small-lake-origin))
									,(+ (cadr x) (cadr small-lake-origin))))
								'((0 0) (0 1) (1 0) (1 1)))
								'(nil nil nil nil)))
		  (index 0))
	  (mapcar (lambda (x)
				(let ((rock nil) (forest nil) (small (nth index small-coords)))
				  (mapcar (lambda (y) (if (intersection '(forest woods)
											(aref biomes (car y) (cadr y)))
										(push y forest)
										(push y rock)))
						  (append (make-absolute x)
								  (if small
									`(,small)
									nil)))
				  (handle-water-list rock +rock-lake-screens+ index)
				  (handle-water-list forest +forest-lake-screens+ index)
				  (incf index)))
			  base-offs)
	  (setf index 0)
	  (mapcar (lambda (x)
				(when x
				  (let* ((absolute `(,(+ (caar lake-info) (car x))
									 ,(+ (cadar lake-info) (cadr x))))
						 (filler (nth index 
									 (if (intersection '(forest woods) 
													   (aref biomes 
															 (car absolute) 
															 (cadr absolute)))
										         +forest-lake-internals+
										         +rock-lake-internals+))))
							(set-new-screen `(,(car absolute)
											  ,(cadr absolute))
											(car filler) (cdr filler))))
				(incf index))
			  int-offs))))
;The above base-offs are in order tl tr bl br t l r b 
;The above internal-offs are in order tl tr bl br

;Note: may want to add forested river-bends for when the river-bend is
;a woods or forest tile (only need to change left section)
;Note: Currently, there are certain cases in which the waterfall is immediately
;adjacent to the river-bend. In these cases, some edge adaptation will need
;to be done to make them fit together.
(defun river-in-screens (river-in-list river-in-info)
  (let ((waterfall `(,(aref *name-screens* 25) . 25))
		(main-river `(,(aref *name-screens* 24) . 24))
		(pre-bend `(,(aref *name-screens* 23) . 23))
		(top-river-bend `(,(aref *name-screens* 22) . 22))
		(bot-river-bend '((165 164 200 144 8 8 8 8 8 river-bend 115 115 
						   115 115 115 115) . 128))
		(horiz-mouth '((water water water water water water water 180
						176 176 115 115 115 115 115 115) . 127))
		(inverter (if (eq (car river-in-info) 'right)
					(lambda (x) x)
					(lambda (x) (invert-screen x))))
		(negator (if (eq (car river-in-info) 'right)
				   (lambda (x) x)
				   (lambda (x) (- x))))
		(dir (if (eq (car river-in-info) 'right)
			   (lambda (x) `(,(caaddr river-in-info)
							 ,(+ x (cadadr (cdr river-in-info)))))
			   (lambda (x) `(,(caaddr river-in-info)
							 ,(- (cadadr (cdr river-in-info)) x)))))
		(mouth-coords (cadddr river-in-info))
		(neighbor-coords (caddr river-in-info)))
	(cond ((eql (car neighbor-coords) (car mouth-coords))
		   (set-new-screen mouth-coords
						   (funcall inverter (car horiz-mouth))
						   (funcall negator (cdr horiz-mouth)))
		   (set-new-screen neighbor-coords
						   (funcall inverter (car pre-bend))
						   (funcall negator (cdr pre-bend))))
		  ((< (car neighbor-coords) (car mouth-coords))
		   (set-new-screen neighbor-coords
						   (funcall inverter (car top-river-bend))
						   (funcall negator (cdr top-river-bend)))
		   (set-new-screen (funcall dir 1)
						   (funcall inverter (car pre-bend))
						   (funcall negator (cdr pre-bend)))
		   (setf 
			 (aref
			   (aref *new-screens* (car mouth-coords) (cadr mouth-coords))
			   (if (eq (car river-in-info) 'right) 6 9))
			 'water)
		   (setf 
			 (aref
			   (aref *new-screens* (car mouth-coords) (cadr mouth-coords))
			   (if (eq (car river-in-info) 'right) 7 8))
			 (if (eq (car river-in-info) 'right) 195 246)))
		  ((> (car neighbor-coords) (car mouth-coords))
		   (set-new-screen neighbor-coords
						   (funcall inverter (car bot-river-bend))
						   (funcall negator (cdr bot-river-bend)))
		   (set-new-screen (funcall dir 1)
						   (funcall inverter (car pre-bend))
						   (funcall negator (cdr pre-bend)))
		   (setf 
			 (aref
			   (aref *new-screens* (car mouth-coords) (cadr mouth-coords))
			   (if (eq (car river-in-info) 'right) 9 6))
			 'water)
		   (setf 
			 (aref
			   (aref *new-screens* (car mouth-coords) (cadr mouth-coords))
			   (if (eq (car river-in-info) 'right) 8 7))
			 (if (eq (car river-in-info) 'right) 134 146))))
	(let ((low (if (eql (car neighbor-coords) (car mouth-coords)) 1 2))
		  (high (- (cadr river-in-info) 2)))
	  (when (>= high low)
		(mapc (lambda (x)
				(set-new-screen (funcall dir x) 
								(car main-river) (cdr main-river)))
			  (interval low high))))
	(set-new-screen (funcall dir (1- (cadr river-in-info)))
					(funcall inverter (car waterfall)) 
					(funcall negator (cdr waterfall)))))

(defun coast-screens (coast-list coast-info horiz-edges vert-edges width height
								 biomes)
  (let ((s-endcap `(,(aref *name-screens* 117) . 117))
		(n-endcap '((167 166 1 129 167 241 169 bl-rock 54 54 54 54 82 82 54 54)
					. 129))
		(u-endcap '((0 241 169 16 200 199 160 4 4 n-rock-water-wall 
					 n-rock-water-wall n-rock-water-wall n-rock-water-wall 
					 n-rock-water-wall n-rock-water-wall n-rock-water-wall) 
					. 133))
		(d-endcap '((0 167 241 169 2 16 2 8 8 s-rock-water-wall 
					 s-rock-water-wall s-rock-water-wall s-rock-water-wall 
					 s-rock-water-wall s-rock-water-wall s-rock-water-wall) 
					. 132))
		(l-bridge '((151 145 81 81 151 88 88 88 246 br-water 
					 18 water 18 water 18 18) . 130))
		(r-bridge '((18 18 water 18 water 18 bl-water 88 88 88 71 71 71 133
					145 81) . 131))
		(screen-types (mapcar (lambda (x) nil) (interval 1 8))))
		;screens in order (tl tr bl br t l r b)
	(labels ((is-ocean (edge)
					   (member edge '(water-wall none)))
			 (is-endcap (coord)
						(let ((succ (cond ((and (eql (car coord) 0)
												(< (cadr coord) (1- width)))
										   `(,(car coord) ,(1+ (cadr coord))))
										  ((eql (cadr coord) 0)
										   `(,(1- (car coord)) ,(cadr coord)))
										  ((eql (car coord) (1- height))
										   `(,(car coord) ,(1- (cadr coord))))
										  ((eql (cadr coord) (1- width))
										   `(,(1+ (car coord)) ,(cadr coord)))))
							  (pred (cond ((and (eql (car coord) 0)
												(> (cadr coord) 0))
										   `(,(car coord) ,(1- (cadr coord))))
										  ((eql (cadr coord) (1- width))
										   `(,(1- (car coord)) ,(cadr coord)))
										  ((eql (car coord) (1- height))
										   `(,(car coord) ,(1+ (cadr coord))))
										  ((eql (cadr coord) 0)
										   `(,(1+ (car coord)) 
											  ,(cadr coord))))))
						  (if (and succ pred)
							(some (lambda (x) (not (member 'coast
														   (aref biomes (car x)
																 (cadr x)))))
								  `(,pred ,succ))
							nil)))
			 (handle-endcap (coord)
							(cond ((equal coord '(0 0))
								   (if (member '(0 1) coast-list
											   :test #'equal)
									 (set-new-screen coord (car n-endcap)
													 (cdr n-endcap))
									 (set-new-screen coord 
													 (invert-screen
													   (car u-endcap))
													 (- (cdr u-endcap)))))
								  ((equal coord `(0 ,(1- width)))
								   (if (member `(0 ,(- width 2)) coast-list
											   :test #'equal)
									 (set-new-screen coord 
													 (invert-screen 
													   (car n-endcap))
													 (- (cdr n-endcap)))
									 (set-new-screen coord (car u-endcap)
													 (cdr u-endcap))))
								  ((equal coord `(,(1- height) 0))
								   (if (member `(,(1- height) 1) coast-list
											   :test #'equal)
									 (set-new-screen coord 
													 (car s-endcap)
													 (cdr s-endcap))
									 (set-new-screen coord 
													 (invert-screen 
													   (car d-endcap))
													 (- (cdr d-endcap)))))
								  ((equal coord `(,(1- height) ,(1- width)))
								   (if (member `(,(1- height) ,(- width 2)) 
											   coast-list :test #'equal)
									 (set-new-screen coord 
													 (invert-screen
													   (car s-endcap))
													   (- (cdr s-endcap)))
									 (set-new-screen coord 
													 (car d-endcap)
													 (cdr d-endcap))))
								  ((eql (car coord) 0)
								   (if (member `(0 ,(1- (cadr coord)))
											   coast-list :test #'equal)
									 (set-new-screen coord
													 (invert-screen 
													   (car n-endcap))
													 (- (cdr n-endcap)))
									 (set-new-screen coord
													 (car n-endcap)
													 (cdr n-endcap))))
								  ((eql (car coord) (1- height))
								   (if (member `(,(car coord) 
												  ,(1- (cadr coord)))
											   coast-list :test #'equal)
									 (set-new-screen coord
													 (invert-screen
													   (car s-endcap))
													 (- (cdr s-endcap)))
									 (set-new-screen coord
													 (car s-endcap)
													 (cdr s-endcap))))
								  ((eql (cadr coord) 0)
								   (if (member `(,(1- (car coord)) 0)
											   coast-list :test #'equal)
									 (set-new-screen coord
													 (invert-screen
													   (car d-endcap))
													 (- (cdr d-endcap)))
									 (set-new-screen coord
													 (invert-screen
													   (car u-endcap))
													 (- (cdr u-endcap)))))
								  ((eql (cadr coord) (1- width))
								   (if (member `(,(1- (car coord)) 
												  ,(cadr coord))
											   coast-list :test #'equal)
									 (set-new-screen coord
													 (car d-endcap)
													 (cdr d-endcap))
									 (set-new-screen coord
													 (car u-endcap)
													 (cdr u-endcap))))))
			 (handle-screen (coord)
							(let* ((edges (collect-edges horiz-edges vert-edges
														 (car coord)
														 (cadr coord)))
								   (edge-count (count-if #'is-ocean edges)))
							  (cond
								((member 'bridge edges) ;bridge
								 (if (eq 'bridge (caddr edges)) 
								   (set-new-screen coord (car r-bridge)
												   (cdr r-bridge))
								   (set-new-screen coord (car l-bridge)
												   (cdr l-bridge))))
								((member 'dock edges) ;dock
								 (let ((screen
										 (if (member (caddr edges) 
													 '(water-wall none)) 
										   `(,(invert-screen 
												(aref *name-screens* 61)) 
											  . -61)
										   `(,(aref *name-screens* 61) . 61))))
								   (set-new-screen coord (car screen) 
												   (cdr screen))))
								((is-endcap coord) ;endcap
								 (handle-endcap coord))
								((and (eq (car coast-info) 'bottom)
									  ;bottom coast-cluster east endcap
									  (equal 
										coord
										`(,(funcall 
											 (caddr coast-info)
											 (if (member
													`(,(funcall 
														(caddr coast-info) 2)
													 ,(funcall 
														(cadddr coast-info) 0))
													coast-list :test #'equal)
											   2 1))
										  ,(funcall (cadddr coast-info) 0))))
								 (set-new-screen coord
												 (invert-screen (car u-endcap))
												 (- (cdr u-endcap))))
								((and (eq (car coast-info) 'bottom)
									  ;bottom coast-cluster west endcap
									  (equal 
										coord
										`(,(funcall 
											 (caddr coast-info)
											 (if (member
													`(,(funcall 
														(caddr coast-info) 2)
													 ,(funcall 
														(cadddr coast-info) 2))
													coast-list :test #'equal)
											   2 1))
										  ,(funcall (cadddr coast-info) 2))))
								 (set-new-screen coord
												 (car u-endcap)
												 (cdr u-endcap)))
								((and (eq (car coast-info) 'bottom-right)
									  (eq (cadr coast-info) 'horizontal)
									  (equal 
										coord 
										`(,(funcall (caddr coast-info) 2)
										  ,(funcall (cadddr coast-info) 1))))
								 (set-inverse-screen coord (car u-endcap)
													 (cdr u-endcap)))
								((and (eq (car coast-info) 'bottom-right)
									  (eq (cadr coast-info) 'horizontal)
									  (equal 
										coord 
										`(,(funcall (caddr coast-info) 1)
										  ,(funcall (cadddr coast-info) 3))))
								 (set-new-screen coord (car u-endcap)
													 (cdr u-endcap)))
								((and (eq (car coast-info) 'bottom-left)
									  (eq (cadr coast-info) 'horizontal)
									  (equal 
										coord 
										`(,(funcall (caddr coast-info) 2)
										  ,(funcall (cadddr coast-info) 1))))
								 (set-new-screen coord (car u-endcap)
													 (cdr u-endcap)))
								((and (eq (car coast-info) 'bottom-left)
									  (eq (cadr coast-info) 'horizontal)
									  (equal 
										coord 
										`(,(funcall (caddr coast-info) 1)
										  ,(funcall (cadddr coast-info) 3))))
								 (set-inverse-screen coord (car u-endcap)
													 (cdr u-endcap)))
								((eql 2 edge-count) ;internal
								 (let ((filler
										 (nth
										   (if (is-ocean (car edges))
											 (if (is-ocean (caddr edges)) 3 2)
											 (if (is-ocean (caddr edges)) 1 0))
										   +coast-internals+)))
								   (set-new-screen coord (car filler)
												   (cdr filler))))
								((eql 1 edge-count) ;flat
								 (let ((water-pos 
										 (position-if #'is-ocean edges))
									   (opp-edge #(1 0 3 2)))
								   (if (eq 'wall 
										   (nth (aref opp-edge water-pos)
												edges))
									 (apply #'set-new-screen coord
											(nth water-pos
												 '((8 124)
												   (7 118)
												   (4 -75)
												   (3 75))))
									 (push coord (nth (nth water-pos '(7 4 6 5))
													  screen-types)))))
								(t ;corner
								  (push
									coord
									(nth
									  (cond
										((is-ocean (west-edge
													 horiz-edges vert-edges
													 (1- (car coord))
													 (cadr coord))) 3)
										((is-ocean (east-edge
													 horiz-edges vert-edges
													 (1- (car coord))
													 (cadr coord))) 2)
										((is-ocean (west-edge
													 horiz-edges vert-edges
													 (1+ (car coord))
													 (cadr coord))) 1)
										((is-ocean (east-edge
													 horiz-edges vert-edges
													 (1+ (car coord))
													 (cadr coord))) 0)
										((is-ocean (north-edge
													 horiz-edges vert-edges
													 (car coord)
													 (1- (cadr coord)))) 3)
										((is-ocean (south-edge
													 horiz-edges vert-edges
													 (car coord)
													 (1- (cadr coord)))) 1)
										((is-ocean (north-edge
													 horiz-edges vert-edges
													 (car coord)
													 (1+ (cadr coord)))) 2)
										((is-ocean (south-edge
													 horiz-edges vert-edges
													 (car coord)
													 (1+ (cadr coord)))) 0))
									  screen-types)))))))
	  (mapc #'handle-screen coast-list)
	  (mapc (lambda (pos-lst side-num)
			  (handle-water-list pos-lst +coast-screens+ side-num))
			screen-types
			(interval 0 7)))))



(defun grave-screens (grave-list grave-info)
  (flet ((shift-coord (y x) `(,(+ y (caar grave-info))
							  ,(+ x (cadar grave-info)))))
	(let ((nw (aref *name-screens* 31))
		  (n '(2 2 7 130 7 7 130 7 7 130 7 7 130 7 2 2))
		  (ne (aref *name-screens* 32))
		  (w (aref *name-screens* 47))
		  (e (aref *name-screens* 48))
		  (sw (aref *name-screens* 62))
		  (s '(2 2 8 147 8 8 147 8 8 147 8 8 147 8 2 2))
		  (se (aref *name-screens* 63)))
	  (set-new-screen (car grave-info) nw 31)
	  (if (zerop (cadr grave-info))
		(progn ;vertical graveyard
		  (set-new-screen (shift-coord 0 1) ne 32)
		  (set-new-screen (shift-coord 1 0) w 47)
		  (set-new-screen (shift-coord 1 1) e 48)
		  (set-new-screen (shift-coord 2 0) sw 62)
		  (set-new-screen (shift-coord 2 1) se 63))
		(progn ;horizontal graveyard
		  (set-new-screen (shift-coord 0 1) n 134)
		  (set-new-screen (shift-coord 0 2) ne 32)
		  (set-new-screen (shift-coord 1 0) sw 62)
		  (set-new-screen (shift-coord 1 1) s 135)
		  (set-new-screen (shift-coord 1 2) se 63))))))

(defun desert-screens (desert-list desert-info)
  (flet ((apply-offset (offset)
					   `(,(+ (car offset) (caar desert-info))
						 ,(+ (cadr offset) (cadar desert-info)))))
	(if (member (caadr desert-info) '(-1 2))
	  (set-new-screen (apply-offset (cadr desert-info))
					  '(0 0 197 197 197 197 197 197 197 197 197 197 197 197 
						0 0) 136)
	  (set-new-screen (apply-offset (cadr desert-info))
					  (aref *name-screens* 40) 40))
	(set-new-screen (apply-offset '(0 0)) (aref *name-screens* 41) 41)
	(set-new-screen (apply-offset '(0 1)) (aref *name-screens* 42) 42)
	(set-new-screen (apply-offset '(1 0)) (aref *name-screens* 57) 57)
	(set-new-screen (apply-offset '(1 1)) (aref *name-screens* 58) 58)))


;28, 71, 93, 103 added to simplify split-screen connections. Maybe remove later.
(defconstant +bad-river-out-screens+ '(4 5 8 15 20 28 34 43 59 69 71 78 83 84
									   88 93 97 99 103 104 105 107 109 110 116))

(defun river-out-screens (river-out-list biomes river-out-info)
  (flet ((handle-one-screen (coord)
							(unless (aref *new-screens* (car coord) 
										  (cadr coord))
							  (let* ((main-biome 
									  (cdr (assoc 
										(find-if 
										  (lambda (x)
											(member x '(mountain hills
														forest woods)))
										  (aref biomes (car coord)
												(cadr coord)))
										`((mountain . ,*mountain-screens*)
										  (hills . ,*hill-screens*)
										  (woods . ,*woods-screens*)
										  (forest . ,*forest-screens*)))))
									 (valid-screens
									   (remove-if 
										 (lambda (x)
											(member x 
												+bad-river-out-screens+))
										 main-biome))
									 (choice (nth 
											   (mrandom (length valid-screens))
											   valid-screens)))
								(set-new-screen 
								  coord
								  (aref *name-screens* choice)
								  choice)
								(pop-nth main-biome 
										 (position choice main-biome))))
							(when (member (get-arr2d *new-screens* coord)
										  '(0 3 4 7 8) :test #'eql)
							  (set-arr2d *new-screens* coord 
										 (coerce
										   (copy-seq
											 (aref +init-duplicate-screens+
											   (get-arr2d *new-screens* 
														  coord))) 'array)))
							(if (numberp (aref *new-screens* (car coord)
												   (cadr coord)))
							  (set-arr2d biomes coord
										 (remove 'river-out 
												 (get-arr2d biomes coord)))
							(let ((river-core
									(if (eql 1 (abs (- (car coord) 
													   (caar river-out-info))))
									  18 'water)))
							(mapc (lambda (x)
									(setf (aref (aref *new-screens* (car coord)
													  (cadr coord)) x)
										  river-core))
								  '(5 6))
							(mapc (lambda (x)
									(mapc (lambda (st repl)
											(when (member (aref 
													  (get-arr2d *new-screens*
															     coord) x)
														  st)
											  (setf (aref 
													  (get-arr2d *new-screens*
															 coord) x) repl)))
										  '((tl-rock tr-rock) 
											(tl-rock2 tr-rock2)
											(tl-tree) (tr-tree)
											(bl-rock br-rock)
											(tl-open tr-open)
											(bl-open br-open)
											(bl-tree br-tree))
										  '(71 81 66 97 54 88 82 72)))
								  '(4 7))
							(when (equal coord (car river-out-info))
							  (mapc (lambda (x y)
									  (setf (aref (aref *new-screens* 
														(car coord)
														(cadr coord)) x) y))
									'(4 7)
									(if (eq 'up (cadr river-out-info))
									  '(246 195) '(134 146))))))))
	(mapc #'handle-one-screen (cons (car river-out-info) river-out-list))))

(defun duplicate-screens (biomes horiz vert width height)
  (let ((duplicates nil)
		(possible-duplicates 
		  (remove-if (lambda (x) (not (and (equal (aref biomes (car x) (cadr x))
												  '(mountain))
										   (equal (get-screen-biome biomes
															(car x) 
															(1- (cadr x)))
												  '(mountain))
										   (equal (get-screen-biome biomes
															(car x) 
															(1+ (cadr x)))
												  '(mountain))
										   (member (north-edge horiz vert 
														   (car x) (cadr x))
											   '(wall none))
										   (member (south-edge horiz vert
														   (car x) (cadr x))
											   '(wall none))
										   (eq (west-edge horiz vert 
														  (car x) (cadr x))
											   'edge)
										   (eq (east-edge horiz vert 
														  (car x) (cadr x))
											   'edge)
										   (eq (aref *new-screens* 
													 (car x) (cadr x))
											   nil))))
					 (get-coords-list width height))))
	(mapc (lambda (x)
			(let ((screen (aref *new-screens* (car x) (cadr x))))
			  (when (numberp screen)
				(if (assoc screen duplicates)
				  (push x (cdr (assoc screen duplicates)))
				  (push `(,screen ,x) duplicates)))))
		  (get-coords-list width height))
	(let ((duplicate-count
			(apply #'+ (mapcar (lambda (x) (1- (length (cdr x))))
							   duplicates)))
		  (mountain-dup-count 0))
	  (labels ((fill-mountain-dup ()
								(when (< duplicate-count 6)
								(princ 'filling-mountain-dup) (fresh-line)
								(let ((dup-num 
										(mrandom (length possible-duplicates))))
								  (princ dup-num) (fresh-line)
								  (set-new-screen (nth dup-num 
													   possible-duplicates)
												  0 6)
								  (incf mountain-dup-count)
								  (setf possible-duplicates
										(append (subseq possible-duplicates
														0 dup-num)
												(subseq possible-duplicates
														(1+ dup-num))))
								  (if (eql 1 mountain-dup-count)
									(fill-mountain-dup)
									(incf duplicate-count))))))
	  (when (< duplicate-count 6)
		(mapc (lambda (x) (fill-mountain-dup))
			  (interval 0 (- 6 duplicate-count))))
	  (when (and (> (length (cdr (assoc 1 duplicates))) 1)
				 (or (> duplicate-count 6)
					 (and (< mountain-dup-count 4) 
						  (> (length possible-duplicates) 
							 (if (zerop mountain-dup-count) 1 0)))))
		(set-new-screen (nth (mrandom (length (cdr (assoc 1 duplicates))))
							 (cdr (assoc 1 duplicates)))
						(aref *name-screens* 33) 33)
		(decf duplicate-count)
		(fill-mountain-dup))
	  (let ((ladder-fairies
			  (remove-if (lambda (x) (not (member 'mountain
												  (get-screen-biome 
													biomes
													(1+ (car x)) (cadr x)))))
						 (cdr (assoc 56 duplicates)))))
		(if (eql (length ladder-fairies) (length (cdr (assoc 56 duplicates))))
		  (mapc (lambda (x) (set-new-screen x 9 123)) ladder-fairies)
		  (when (and (< 0 (length ladder-fairies))
					 (or (> duplicate-count 6)
						 (and (< mountain-dup-count 4) 
							  (> (length possible-duplicates) 
								 (if (zerop mountain-dup-count) 1 0)))))
			(mapc (lambda (x) (set-new-screen x 9 123)) ladder-fairies)
			(decf duplicate-count)
			(fill-mountain-dup))))
	  (let ((east-bends
			  (remove-if (lambda (x) (eq 'edge (east-edge horiz vert
														  (car x) 
														  (1+ (cadr x)))))
						 (cdr (assoc 7 duplicates))))
			(west-bends
			  (remove-if (lambda (x) (eq 'edge (west-edge horiz vert
														  (car x) 
														  (1- (cadr x)))))
						 (cdr (assoc 7 duplicates)))))
		(when (and (or (> duplicate-count 6)
					   (and (< mountain-dup-count 3) 
							(> (length possible-duplicates)
							   (if (zerop mountain-dup-count) 1 0))))
				   (> (+ (length east-bends) (length west-bends)) 0))
		  (mapc (lambda (x)
				  (let ((choice (mrandom (+ (length east-bends) 
											(length west-bends)))))
					(if (< choice (length east-bends))
					  (progn (set-new-screen (nth choice east-bends)
											 (aref *name-screens* 119) 119)
							 (setf east-bends
								   (append (subseq east-bends 0 choice)
										   (subseq east-bends (1+ choice)))))
					  (progn (setf choice (- choice (length east-bends)))
							 (set-inverse-screen (nth choice west-bends)
												 (aref *name-screens* 119) 119)
							 (setf west-bends
								   (append (subseq west-bends 0 choice)
										   (subseq west-bends (1+ choice))))))
					(decf duplicate-count))
				  (fill-mountain-dup))
				(interval 1 (min (+ (- duplicate-count 6)
									(if (zerop mountain-dup-count)
									  (min 2 (1- (length possible-duplicates)))
									  (min (- 3 mountain-dup-count)
										   (1- (length possible-duplicates)))))
								 (+ (length east-bends) 
									(length west-bends)))))))
	  (let ((n-coasts (remove-if 
						(lambda (x) (not 
									  (or
										(eql 81 (aref *new-base-screens*
													  (car x) (1+ (cadr x))))
										(eql 81 (aref *new-base-screens*
													  (car x) (1- (cadr x)))))))
						(cdr (assoc 8 duplicates)))))
		(when (and (or (> duplicate-count 6)
					   (and (< mountain-dup-count 3) 
							(> (length possible-duplicates) 
							   (if (zerop mountain-dup-count) 1 0))))
				   (> (length n-coasts) 0))
		  (mapc (lambda (x)
				  (let ((choice (mrandom (length n-coasts))))
					(set-new-screen (nth choice n-coasts)
									'(54 54 54 54 54 54 54 54 54 54 54 
									  54 54 54 54 54) 124)
					(decf duplicate-count)
					(setf n-coasts (append (subseq n-coasts 0 choice)
										   (subseq n-coasts (1+ choice)))))
				  (fill-mountain-dup))
				(interval 1 (min (+ (- duplicate-count 6)
									(if (zerop mountain-dup-count)
									  (min 2 (1- (length possible-duplicates)))
									  (min (- 3 mountain-dup-count)
										   (1- (length possible-duplicates)))))
								 (length n-coasts)))))))
	  (princ "duplicate count: ") (princ duplicate-count) (fresh-line)
	  (princ duplicates) (fresh-line))))


(defun fill-remaining-screens (remaining-list biomes)
  (let ((screens `(,*mountain-screens* ,*hill-screens* ,*woods-screens*
									   ,*forest-screens*))
		(sources `(,+mountain-screens+ ,+hill-screens+ ,+woods-screens+
									   ,+forest-screens+))
		(names '(mountain hills woods forest)))
  (labels ((check-and-refill (lst-num)
							 (unless (nth lst-num screens)
							   (setf (nth lst-num screens) 
									 (copy-seq (nth lst-num sources)))))
		   (fill-one-screen (coord)
							(unless (aref *new-screens* (car coord) 
										  (cadr coord))
							(let* ((biome-num (position
											   (find-if
												 (lambda (x) (member x names))
												 (aref biomes (car coord)
													   (cadr coord)))
											   names))
								   (choice (pop-nth (nth biome-num screens)
													(mrandom 
													  (length
														(nth biome-num 
															 screens))))))
							  (set-new-screen coord
											  (aref *name-screens* choice)
											  choice)
							  (check-and-refill biome-num)))))
	(mapc #'check-and-refill '(0 1 2 3))
	(mapc #'fill-one-screen remaining-list))))
