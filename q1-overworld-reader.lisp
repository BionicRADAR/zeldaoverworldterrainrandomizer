;Reads the overworld off the actual NES Legend of Zelda rom
;Also contains code for displaying it in a visual form.
(load "read-rom.lisp")

(defparameter *columns* nil)
(defparameter *screens* nil)
(defparameter *q1-map* nil)
(defparameter *bush-tile* #x19)
(defparameter *b-bush-tile* #x28)
(defparameter *wall-tile* #x1B)
(defparameter *b-wall-tile* #x27)
(defparameter *cave-tile* #x0C)
(defparameter *ladder-tile* #x0A)
(defparameter *empty-tile* #x0E)
(defparameter *desert-tile* #x37)
(defparameter *dock-tile* #x0B)
(defparameter *tl-wall-tile* #x35)
(defparameter *tr-wall-tile* #x34)
(defparameter *bl-wall-tile* #x33)
(defparameter *br-wall-tile* #x32)
(defparameter *armos-tile* #x2C)
(defparameter *armos2-tile* #x2B)
(defparameter *armos3-tile* #x2A)
(defparameter *boulder-tile* #x13)
(defparameter *m-boulder-tile* #x26)
(defparameter *grave-tile* #x14)
(defparameter *m-grave-tile* #x29)
(defparameter *cave-tile-sub-list* `((,*b-bush-tile* ,*bush-tile*) 
									 (,*b-wall-tile* ,*wall-tile*)
									 (,*cave-tile* ,*wall-tile*)
									 (,*m-boulder-tile* ,*boulder-tile*)
									 (,*m-grave-tile* ,*grave-tile*)))
(defparameter *cave-col-sub-list* (make-array 150))
(defparameter *bomb-repl* nil)
(defparameter *open-repl* nil)
(defparameter *burn-repl* nil)
(defparameter *grave-repl* nil)
(defparameter *boulder-repl* nil)

(defparameter *edge-pair-list* '(((100 216) (102 242)) ;high stump
								 ((229 242) (243 216)) ;low stump
								 ((103) (112))))	   ;ruin spire

;These variables were just for displaying the q1 overworld using
;zelda-display.lisp
(defparameter *q1-specials*
  (append (loop repeat 11 collect nil) '((11 4))
			   (loop repeat 16 collect nil) '((11 4)) '((5 3))
			   (loop repeat 4 collect nil) '((3 4)) '((4 5)) '((14 4))
			   (loop repeat 15 collect nil) '((4 4))
			   (loop repeat 8 collect nil) '((9 4) nil nil nil nil recorder)
			   (loop repeat 6 collect nil) '((5 3))
			   (loop repeat 4 collect nil) '((10 4))
			   (loop repeat 42 collect nil) '((9 5))
			   (loop repeat 6 collect nil)))
(defparameter *q1-palettes* nil)

(defun fill-map (fname)
  (with-open-file (zelda fname :direction :input 
						 :element-type '(unsigned-byte 8))
	(loop repeat 87080 do (read-byte zelda))
	(setf *screens* (coerce (loop repeat 124 collect
			(loop repeat 16 collect (read-byte zelda))) 'array))
	(setf *columns* (coerce (get-columns zelda) 'array))
	(loop repeat 9316 do (read-byte zelda))
	(setf *q1-palettes* 
		  (flet ((hex-to-palette (x)
					(cond ((zerop (mod x 4)) 'gray)
						  ((eql 1 (mod x 4)) 'orange)
						  ((eql 2 (mod x 4)) 'green)
						  (t 'brown))))
			(mapcar (lambda (x y) (list x y))
					(mapcar #'hex-to-palette
							(loop repeat 128 collect (read-byte zelda)))
					(mapcar #'hex-to-palette
							(loop repeat 128 collect (read-byte zelda))))))
	(loop repeat 128 do (read-byte zelda))
	(setf *q1-map* (mapcar (lambda (x) (if (> x #x80) (- x #x80) x)) 
						   (loop repeat 128 collect (read-byte zelda))))))

(defun fill-cave-subs (columns)
  (let ((cave-tiles (mapcar #'car *cave-tile-sub-list*)))
	(labels ((replace-cave (col)
						   (mapcar
							 (lambda (x)
							   (let ((rep (assoc x *cave-tile-sub-list*)))
								 (if rep
								   (cadr rep)
								   x)))
							 col))
			 (find-cave (col)
						(find-if (lambda (x) (member x cave-tiles)) col))
			 (find-column (col)
						  (loop for x below (length columns)
								when (equal col (aref columns x))
								return x))
			 (get-replacement (col-no)
							  (let ((col (aref columns col-no)))
							  (if (find-cave col)
								(let ((repl (find-column (replace-cave col))))
								  (if repl
									repl
									col-no))
								col-no)))
			 (handle-column (col-no)
							(let ((repl (rev-convert-col-num 
										  (get-replacement col-no)))
								  (conv-col-num (rev-convert-col-num col-no)))
							  (setf (aref *cave-col-sub-list* col-no) repl)
							  (unless (eql conv-col-num repl)
								(let ((tile (find-cave (column-by-number
														conv-col-num)))
									  (repl-pair `(,repl ,conv-col-num)))
								  (cond ((eq tile *b-bush-tile*)
										 (push repl-pair *burn-repl*))
										((eq tile *b-wall-tile*)
										 (push repl-pair *bomb-repl*))
										((eq tile *cave-tile*)
										 (push repl-pair *open-repl*))
										((eq tile *m-grave-tile*)
										 (push repl-pair *grave-repl*))
										((eq tile *m-boulder-tile*)
										 (push repl-pair *boulder-repl*))
										(t (error "Tile not replaceable"))))))))
	  (mapc #'handle-column (loop for x below 150 collect x)))))

								
		
(defun get-columns (zelda-stream)
  (let ((columns-list (loop repeat 964 collect (read-byte zelda-stream))))
	(labels ((step-list (lst acc)
		  (if (endp lst)
			(reverse acc)
		    (if (> (car lst) #x80)
		      (step-list (cdr lst) (cons (parse-column lst nil 11) acc))
		      (step-list (cdr lst) acc))))
		(parse-column (lst col len)
		  (if (<= len 0)
			(reverse col)
			(multiple-value-bind (double tile)
			  (floor (car lst) 64)
			  (parse-column (cdr lst)
						    (if (oddp double)
							  (cons tile (cons tile col))
							  (cons tile col))
							(if (oddp double)
							  (- len 2)
							  (1- len)))))))
	  (step-list columns-list nil))))

(defun convert-column-number (num)
  (let ((shift-array '#(0 -1 -1 -1 -1 -1 -2 -3 -4 -5 -6 -6 -7 -8 -9 -10)))
	(multiple-value-bind (high low) (floor num 16)
	  (+ (+ low (aref shift-array high)) (* 10 high)))))

(defparameter *reverse-col-num-convert* (make-array 150))

(loop for x below 250 do (setf (aref *reverse-col-num-convert*
									 (convert-column-number x)) x))

(defun rev-convert-col-num (num)
  (aref *reverse-col-num-convert* num))

(defun column-by-number (num)
  (if (numberp num)
	(aref *columns* (convert-column-number num))
	(new-column-by-number num)))

(defun sub-col-num-by-num (num)
  (aref *cave-col-sub-list* (convert-column-number num)))

(defun get-numbers-used ()
  (let ((was-found (make-array 256)))
	(loop for i below (length was-found)
		  do (setf (aref was-found i) 0))
	(loop for i below (length *screens*)
		  do (mapc (lambda (x) (incf (aref was-found x)))
				(aref *screens* i)))
	(loop for i below (length was-found)
		  unless (zerop (aref was-found i))
		  collect i)))

(defun format-numbers-used ()
  (let ((numbers (get-numbers-used))
		(prev -1))
	(mapc (lambda (x)
			 (when (not (eql x (1+ prev)))
			   (format t "~&"))
			 (format t "~2,'0x " x)
			 (setf prev x)) numbers)))

(fill-map "zelda-rom.nes")

(defstruct biome-info 
		   columns
		   l-exit-hash
		   r-exit-hash
		   l-adj
		   r-adj
		   r-two
		   left-edge
		   right-edge
		   south-freqs
		   s-ladder
		   s-open
		   s-wall
		   n-ladder
		   n-open
		   n-wall
		   screens)

(defun build-biome-info (screens)
  (make-biome-info
	:l-exit-hash (make-hash-table :test #'equal)
	:r-exit-hash (make-hash-table :test #'equal)
	:r-adj (make-array 150)
	:l-adj (make-array 150)
	:r-two (make-array 150)
	:screens screens))

(defun tile-exit (tile side)
  (cond ((member tile `(,*empty-tile* ,*dock-tile* ,*desert-tile*))
			nil)
		((member tile `(,*ladder-tile* ,*armos-tile* ,*armos2-tile*
						,*armos3-tile* ,*boulder-tile* ,*m-boulder-tile*
						,*grave-tile* ,*m-grave-tile*))
			'illegal)
		((and (eq side 'left) (member tile `(,*tr-wall-tile* ,*br-wall-tile*)))
			(if (eq tile *tr-wall-tile*) 'tr 'br))
		((and (eq side 'right) (member tile `(,*tl-wall-tile* ,*bl-wall-tile*)))
			(if (eq tile *tl-wall-tile*) 'tl 'bl))
		(t t)))

(defun reflect-exit-tile (exit-tile)
  (cond ((eq exit-tile 'tl) 'tr)
		((eq exit-tile 'tr) 'tl)
		((eq exit-tile 'bl) 'br)
		((eq exit-tile 'br) 'bl)
		(t exit-tile)))

(defun tree-exit-tile (exit-tile)
  (if (member exit-tile '(tl tr bl br))
	t
	exit-tile))

(defun column-exit (column side)
  (let ((tiles 
		  (mapcar (lambda (x) (tile-exit x side)) (aref *columns* column))))
	(if (and (eq (first tiles) t) (eq (car (last tiles)) t) 
			 (notany (lambda (x) (eq x 'illegal)) tiles))
	  tiles
	  nil)))

(defun get-column-info (col left right r-two biome-info)
  (unless (member col (biome-info-columns biome-info))
	(push col (biome-info-columns biome-info)))
  (when left (freq-list-push left (aref (biome-info-l-adj biome-info) col)))
  (when right (freq-list-push right (aref (biome-info-r-adj biome-info) col))
	(let ((north (tile-type (car (aref *columns* right))))
		  (south (tile-type (nth 10 (aref *columns* right)))))
	  (cond ((eq north 'empty) 
			 (unless (member col (biome-info-n-open biome-info))
			   (push col (biome-info-n-open biome-info))))
			((eq north 'ladder)
			 (unless (member col (biome-info-n-ladder biome-info))
			   (push col (biome-info-n-ladder biome-info))))
			(t (unless (member col (biome-info-n-wall biome-info))
				 (push col (biome-info-n-wall biome-info)))))
	  (cond ((eq south 'empty) 
			 (unless (member col (biome-info-s-open biome-info))
			   (push col (biome-info-s-open biome-info))))
			((eq south 'ladder)
			 (unless (member col (biome-info-s-ladder biome-info))
			   (push col (biome-info-s-ladder biome-info))))
			(t (unless (member col (biome-info-s-wall biome-info))
				 (push col (biome-info-s-wall biome-info)))))))
  (when r-two (freq-list-push r-two (aref (biome-info-r-two biome-info) col)))
  (let ((l-exit (column-exit col 'left))
		(r-exit (column-exit col 'right)))
	(when l-exit
	  (unless (member col (biome-info-left-edge biome-info))
		(push col (biome-info-left-edge biome-info))) 
	  (freq-list-push col (gethash l-exit (biome-info-l-exit-hash biome-info))))
	(when r-exit
	  (unless (member col (biome-info-right-edge biome-info))
		(push col (biome-info-right-edge biome-info))) 
	  (freq-list-push col (gethash r-exit 
								   (biome-info-r-exit-hash biome-info)))))
  (tile-type (car (last (aref *columns* col)))))


(defun get-screen-info (screen biome-info)
  (let* ((conv-screen (mapcar #'convert-column-number screen))
		 (south-exits
		  (mapcar (lambda (c l r r2) (get-column-info c l r r2 biome-info))
				  conv-screen
				  (cons nil (subseq conv-screen 0 (1- (length screen))))
				  (append (cdr conv-screen) `(,nil))
				  (append (cddr conv-screen) `(,nil ,nil)))))
	(unless (every (lambda (x) (eq x 'wall)) south-exits)
	  (push south-exits (biome-info-south-freqs biome-info)))))


(defun get-part-screen-info (screen limits biome-info)
  (get-screen info (subseq screen (car limits) (cadr limits)) biome-info))

(defun fill-biome-info (biome-info)
  (mapc (lambda (x) (get-screen-info (aref *screens* x) biome-info))
		(biome-info-screens biome-info)))
  

(defparameter *mountain-info* (build-biome-info
								 '(0 1 2 3 4 5 6 7 8 11 12 15 16 
								   17 18 19 20 21 26 27 34 35 36 
								   43 49 50 107 108 116)))

;finishes mountain info by adding some s-opens and a double-open
;namely, columns 4 (low s-open), 7 (high s-open), 24 (double-open)
(defun finish-mountain-info (mountain-info)
  (let ((cols (biome-info-columns mountain-info))
		(r-adj (biome-info-r-adj mountain-info))
		(s-open (biome-info-s-open mountain-info))
		(n-open (biome-info-n-open mountain-info))
		(to-add '(4 7 24)))
	(mapc (lambda (x) (push x cols)) to-add)
	(mapc (lambda (y) 
			(mapc (lambda (x) (push (list x 1) (aref r-adj y)) to-add))
			(push y n-open)
			(push y s-open))
		  (remove 0 cols))))

(defparameter *forest-info* (build-biome-info
							  '(59 69 71 73 74 83 84 87 88 89
								99 102 103 104 105 115)))

(defparameter *woods-info* (build-biome-info '(77 78 79 91 92 93 94 109 110)))

(defparameter *hills-info* (build-biome-info '(95 97 98 111 112 113 114)))


;This function has questionable results for armos, pushable blocks, and 
;angled walls
(defun tile-type (tile)
  (cond ((member tile `(,*empty-tile* ,*desert-tile* ,*dock-tile*)) 'empty)
		((eql tile *ladder-tile*) 'ladder)
		(t 'wall)))

(defparameter *all-tile-types* '(empty ladder wall))
(defparameter *num-exits* 0)
;Exit generation notes: I counted ~130 non-special exits
;	Special exits include exits from: open-ruin dungeons
;									  islands
;									  the secret passage
;									  fairies/ponds
;									  the spring
;When I ran the code to count the exits, I came up with 156 total, including
;	special exits

(defun count-exits (screens)
  (labels ((look-for-south-exit (col-num)
				(member (tile-type (nth 10 (column-by-number col-num))) 
						'(ladder empty)))
		   (look-for-east-exit (col-num)
				(some (lambda (x) (eq (tile-type x) 'empty)) 
					  (column-by-number col-num)))
		   (south-exit-screen (screen)
				(if (some #'look-for-south-exit (subseq screen 1 15))
				  1
				  0))
		   (east-exit-screen (screen)
				(if (look-for-east-exit (nth 15 screen))
				  1
				  0)))
	(+ (reduce #'+ (mapcar #'south-exit-screen (coerce screens 'list)))
	   (reduce #'+ (mapcar #'east-exit-screen (coerce screens 'list))))))

(defun find-problem-columns (screens)
  (let ((top-adj (make-array 150))
		(bot-adj (make-array 150)))
	(flet ((adj-top (col right)
					(when (member (tile-type (car (column-by-number right)))
								  '(ladder empty))
					  (setf (aref top-adj (convert-column-number col)) t)))
		   (adj-bot (col right)
					(when (member (tile-type (nth 10 (column-by-number right)))
								  '(ladder empty))
					  (setf (aref bot-adj (convert-column-number col)) t))))
	  (mapcar (lambda (x) 
				(mapcar (lambda (y) (adj-top (nth y x) (nth (1+ y) x))
									(adj-bot (nth y x) (nth (1+ y) x)))
						'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
			  (coerce screens 'list))
	  (values top-adj bot-adj))))

(defun search-two-away (screens)
  (multiple-value-bind (top-adj bot-adj) (find-problem-columns screens)
	(let ((top-2 (make-array 150)) (bot-2 (make-array 150)))
	  (flet ((adj-top (col right)
					(when (aref top-adj (convert-column-number right))
					  (setf (aref top-2 (convert-column-number col)) t)))
			 (adj-bot (col right)
					(when (aref bot-adj (convert-column-number right))
					  (setf (aref bot-2 (convert-column-number col)) t))))
		(mapcar (lambda (x) 
				  (mapcar (lambda (y) (adj-top (nth y x) (nth (1+ y) x))
							(adj-bot (nth y x) (nth (1+ y) x)))
						  '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
				(coerce screens 'list))
		(loop for x below 150 unless (aref top-2 x)
			  do (princ x) (fresh-line))
		(loop for x below 150 unless (aref bot-2 x)
			  do (princ x) (fresh-line))))))

(defparameter *graveyard-bases*
  (let ((nw (aref *screens* 31))
		(ne (aref *screens* 32))
		(w (aref *screens* 47))
		(e (append (subseq (aref *screens* 48) 0 14) '(21 21)))
		(sw (append (subseq (aref *screens* 62) 0 14) '(2 2)))
		(se (aref *screens* 63))
		(n (append '(7 7 7) (subseq (aref *screens* 31) 3)))
		(s (append (subseq (aref *screens* 63) 0 13) '(8 8 8))))
	`((nw ,nw) (ne ,ne) (sw ,sw) (se ,se) (n ,n) (e ,e) (s ,s) (w ,w))))


;The following should disqualify a column from being at a screen edge:
;	Armos statue
;	Potentially pushable object (boulder, grave)
;	Ladder
;	Open in the top or bottom two columns
;	Cave entrance of any kind
;
;(defun make-opening-list (column dir)
;  (let ((open-tiles (append '(*empty-tile* *desert-tile* *dock-tile*)
;							(if (eq dir 'left)
;							  '(*tr-wall-tile* *br-wall-tile*)
;							  '(*tl-wall-tile* *bl-wall-tile*)))))



;Starting here are some notes on biomes; these will be implemented in code
;so I can have models for generated overworld screens
;
;Mountain screens: 00-07, 09, 0C, 0D, 10-16, 1B, 1C, 23-25, 2C, 32, 33, 70, 71,
;				   79
;
;Forest screens: 3D, 49, 4B, 4D, 4E, 57, 58, 5B-5D, 68, 6B-6E, 78
;
;Woods screens: 51-53, 60-63, 72-73
;
;Hills screens: 4A, 4C, 5E, 64, 66, 67, 74, 76, 77
;	ignoring river columns [col 05, 06]: 65, 75 
;
;Coast
;	Line screens: 2E, 4F, 6F, 7B, 7C, 7E, 7F
;	Cluster screens: 0E, 1D-1F, 2D, 3E
;
;Big-Lake
;	Mountain/Hills: 26, 27 [ignore river col 5-6], 34-36, 44, 54 [cols 14+]
;					55 [ignore/alter cols 4-7], 56 [cols 1-]
;	Forest/Woods: 28, 38, 46-48, 54 [cols 13-], 56 [cols 2+]
;  Big Lake can borrow from coast for mountain/hills, small lake for 
;  forest/woods
;  Generally, when the lake is taking up full columns [or completely absent
;  from some columns], the two parts of the  screen [lake/not lake] should 
;  be treated separately
;
;Small-Lake screens: 59, 5A, 69 6A
;	These are all forest screens and will also work for woods
;		28, 48, some of 54, 56 from big-lake can also be used
;	For mountain/hills, should use big-lake corner screen stuff
;		(26 34, some of 54, 56)
;
;Graveyard: 20, 21, 30, 31, 40, 41
;	Grave-external: 50 [also usable for hills/mountain]
;
;Desert: 29-2B, 3A, 3B
;
;River:
;	Spring: 0A
;	Waterfall/In: 1A
;	In Screens: 17-19, 27 [lake]
;	Out Screens: 55 [lake], 56, 57
;
;Island: 0F, 2E, 37, 45 [Docks: 3E, 55]
;	Island 0F is actually the same screen as island 45, but it takes the place
;	of what on the biomes map is an open-ruin. Likely, I should pick an open-
;	ruin to be the secret passage island if I put in a secret passage.
;
;Open-Ruin: 0B, 1F, 22, 3C, 74
;
;A-Fairy: 39 [also appears at 43, but they're the same set of columns
;	with the same palette; if another fairy screen [with a ladder] is required,
;	the second quest dungeon 2 replacement screen works]
;
;Pond: 42 [same screen as a-fairy, different palette, different enemy set]
