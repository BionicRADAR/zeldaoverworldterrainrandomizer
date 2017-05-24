;a utility function to help find appropriate columns; probably shouldn't be
;used in code so it can be removed from final product
;Currently used to find: +cols-not-to-replace+
;	In order to remove from final product, must record all of 
;		+cols-not-to-replace+ in file
(defun find-columns-that (condit)
  (mapcar #'rev-convert-col-num 
		  (remove-if (lambda (x) (not (funcall condit (aref *columns* x))))
					 (interval 0 (1- (length *columns*))))))

(defconstant +ladder-cols+ '(17 56 80 104 131 135 150 179 181 182 184 215))

(defconstant +open-vert-cols+ '(4 7 8 24 25 32 35 bl-open br-open 82 87 88 116 
								118 130 134 136 146 147 148 176 195 208 209 
								211 213 226 227 246))

(defconstant +actual-edge-types+ ;Refers to the furthest left and right columns
								 ;of each screen, where two screens meet
			 #(("Wall: no edge" ())										;0
			   ("Rocky one-wide" (5))									;1
			   ("Rocky open at low half" (5 6 7 8))						;2
			   ("Rocky center 5-wide; angled into" (3 4 5 6 7))			;3
			   ("Rocky center 7-wide" (2 3 4 5 6 7 8))					;4
			   ("Rocky center 7-wide, interrupted by tree" (2 5 6 7 8))	;5
			   ("Rocky, angled center 7-wide" (angle 2 3 4 5 6 7 8))	;6
			   ("Rocky, angled center 5-wide" (angle 3 4 5 6 7))		;7
			   ("Rocky, angled 9-wide" (angle 1 2 3 4 5 6 7 8 9))		;8
			   ("Rocky center 3-wide; angled into" (4 5 6))				;9
			   nil														;10
			   ("Two gaps in forest" (2 8))								;11
			   ("Forested center 3-wide" (4 5 6))						;12
			   ("Four gaps in forest" (2 4 6 8))						;13
			   ("Forested center 7-wide" (2 3 4 5 6 7 8))				;14
			   ("Forested center 5-wide" (3 4 5 6 7))					;15
			   ("Close north coast; 2-wide gap" (4 5))					;16
			   ("Angled close north coast; angle 3-wide gap" (angle 3 4 5));17
			   ("Wide north coast; 4-wide gap" (2 3 4 5))				;18
			   ("Forested low 4-wide" (5 6 7 8))						;19
			   nil														;20
			   ("Wide south coast; 4-wide gap" (5 6 7 8))				;21
			   ("Close rocky south coast; 1-wide gap" (5))				;22
			   ("Close forested south coast; 2-wide gap" (5 6))))		;23

(defconstant +adapt-edge-types+ ;Refers to columns 2 and 13, two from the left
								;and right edges, the last ones unchanged when
								;edges are changed
			 #(nil														;0
			   ("Rocky center 1-wide gap" (5))							;1
			   ("Rocky low 4-wide gap" (5 6 7 8))						;2
			   ("Rocky center 5-wide gap" (3 4 5 6 7))					;3
			   ("Rocky center 7-wide gap" (2 3 4 5 6 7 8))				;4
			   ("Rocky angle center 5-wide gap" (angle 3 4 5 6 7))		;5
			   ("Rocky angle center 9-wide gap" (angle 1 2 3 4 5 6 7 8 9));6
			   ("Rocky center 7-wide gap with tree" (2 3 4 5 8))		;7
			   ("Rocky special screen 107 center 3-wide" (5 6 7))		;8
			   nil														;9
			   nil														;10
			   ("Two gaps in forest" (2 8))								;11
			   ("Forested center 3-wide" (4 5 6))						;12
			   ("Four gaps in forest" (2 4 6 8))						;13
			   ("Forested center 7-wide" (2 3 4 5 6 7 8))				;14
			   ("Forested low 4-gap" (5 6 7 8))							;15
			   ("Rocky close north coast; 2-wide gap" (4 5))			;16
			   ("Angled close north coast; angle 3-wide gap" (angle 3 4 5));17
			   ("Rocky wide north coast; 4-wide gap" (2 3 4 5))			;18
			   ("Forested wide north coast; 4-wide gap" (2 3 4 5))		;19
			   ("Forested close north coast; 2-wide gap" (4 5))			;20
			   ("Wide rocky south coast; 4-wide gap" (5 6 7 8))			;21
			   ("Close rocky south coast; 1-wide gap" (5))				;22
			   ("Wide forested south coast; 4-wide gap" (5 6 7 8))		;23
			   ("Close forested south coast; 2-wide gap" (5 6))			;24
			   ("Desert center 7-wide gap" (2 3 4 5 6 7 8))))			;25


(defparameter *left-type* #((0 1) (1 4) (1 1) (2 6) (0 2) (3 3) (1 1) (1 1) 
							(1 6) (0 4) (0 0) (0 1) (4 6) (0 0) (0 0)
							(0 3) (5 4) (0 2) (5 4) (4 6) (2 2) (6 4) (4 4) 
							nil nil nil (1 3) (1 4) (4 4) (16 16) (4 4)
							(0 4) (4 4) (0 0) (0 4) (4 4) (4 4) (0 4) (17 16)
							(18 19) (4 25) (4 25) (4 25) (4 4) (2 2) (21 21) 
							(0 0)
							(0 4) (4 4) (7 3) (1 4) (0 4) (0 16) (17 17) (0 0)
							(0 0) (0 0) (0 25) (4 25) (0 13) (0 3) (21 21)
							(0 4) (4 4) (0 4) (0 0) (0 0) (23 24) 
							(23 24) (0 14) (7 3) (12 14) (0 4) (12 14) (12 14) 
							(0 2)
							(0 2) (0 14) (12 14) (11 14) (12 14) (22 21) (22 22)
							(14 14)
							(13 13) (12 12) (16 20) (14 14) (11 11) (12 12)
							(9 4) 
							(0 14) (12 12) (12 12) (15 14) (7 3) (4 4) (4 4)
							(9 4) 
							(12 14) (12 14) (21 23) (14 14) (12 12) (11 14) 
							(12 14) (8 4)
							(0 8) (4 4) (12 15) (11 11) (9 4) (0 4) (4 4) 
							(1 2)
							(1 4) (8 2) (1 1) (18 18) (18 17) (16 16)
							nil nil (0 0)
							;screens not in the base game start below
							(21 21) (16 20) (16 20) (0 0) (4 6) (1 1) (16 16) 
							nil (0 7) (0 3) (4 4) (4 4) (0 4)))
(defparameter *right-type* #((1 1) (1 4) (2 2) (0 1) (3 4) (1 4) (1 1) (1 1)
							 (0 3) (0 4) (0 0) (4 3) (0 2) (0 0) (0 0)
							 (5 4) (0 4) (5 7) (4 2) (2 4) (6 4) (4 3) nil 
							 nil nil (1 2) (1 3) (4 4) (16 16) (4 4) (0 0)
							 (4 4) (0 14) (0 0) (4 4) (4 4) (0 4) (17 16) 
							 (18 17) 
							 (14 12) (4 25) (4 25) (4 25) (2 2) (21 21) (0 0) 
							 (0 0)
							 (4 4) (12 14) (1 1) (0 4) (0 18) (17 16) (0 0) nil 
							 (0 14) (0 0) (4 25) (0 25) (0 12) (21 21) (0 0)
							 (4 4) (0 14) (0 0) (0 0) (23 24) (23 24) 
							 (0 14) (12 12) (7 3) (0 14) (9 1) (12 14) (0 14)
							 (0 0)
							 (0 2) (12 12) (11 11) (12 14) (22 21) (22 21) 
							 (14 14) (13 13)
							 (12 14) (16 20) (14 14) (11 14) (12 12) (12 12) 
							 (0 4)
							 (12 14) (12 12) (15 12) (12 14) (4 4) (4 4) (9 4)
							 (9 4) 
							 (12 13) (21 23) (14 14) (12 14) (11 11) (12 12) 
							 (14 14) (0 0)
							 (4 4) (7 3) (11 11) (12 14) (0 4) (4 4) (1 4) (1 2)
							 (14 13) (1 1) (18 18) (18 18) (16 16) (0 0)
							 nil nil (0 0)
							 ;screens not in the base game start below
							 (21 21) (18 19) (0 0) nil nil (21 21) nil 
							 (16 18) (0 0) (0 0) (4 4) (4 4) (0 4)))

;Info on screens not in base game:
;124: flat north coast screen; also used as duplicate screen 8
;125: top forest lake screen
;126: top-left forest lake internal
;127: Horizontal rivermouth
;128: Northward riverbend
;129: Westward, north-coast endcap
;130: Left-land coast bridge
;131: Right-land coast bridge
;132: Downward endcap (east coast)
;133: Upward endcap (east coast)
;134: North grave
;135: South grave
;136: North-south desert offshoot


;This array and the following function (type-to-adapt) are here to handle
;that many types can resolve to the same value, and that it may be useful
;to refer to different-named adapt arrays conveniently, as I have here, while
;maintaining only one type array for each side for the whole map.
;mountain-type-to-adapt should be accessed with a relevant mountain-type from
;the above left-type and right-type arrays to receive the adapt number for
;that type, while type-to-adapt should work similarly, but universally for
;all types of different biomes.
(defparameter *mountain-type-to-adapt* #(0 1 2 3 4 4 4 5 6 7 8))

(defparameter *base-scr-biomes*
  #(mountain mountain mountain mountain									;row 0
	mountain mountain mountain mountain									;row 0
	mountain spring open-ruin											;row 0
	mountain mountain (coast mountain) open-ruin						;row 0
	mountain mountain mountain mountain									;row 1
	mountain mountain mountain (river-in mountain)						;row 1
	river-in river-in (river-in mountain) mountain						;row 1
	mountain (coast mountain) (coast mountain) open-ruin				;row 1
	grave grave open-ruin mountain										;row 2
	mountain mountain (big-lake mountain) (big-lake river-in mountain)	;row 2
	(big-lake woods) desert desert desert								;row 2
	mountain (coast mountain) coast island								;row 2
	grave grave mountain mountain										;row 3
	(big-lake mountain) (big-lake mountain) (big-lake mountain) island	;row 3
	(big-lake woods) a-fairy desert desert								;row 3
	forest coast coast													;row 3
	grave grave															;row 4
	(big-lake mountain) island (big-lake forest) (big-lake forest)		;row 4
	(big-lake forest) forest hills forest								;row 4
	hills forest forest coast											;row 4
	(grave mountain) woods woods woods									;row 5
	(big-lake forest) (big-lake river-out hills) (big-lake forest) forest;row 5
	forest (small-lake forest) (small-lake forest) forest				;row 5
	forest forest hills													;row 5
	woods woods woods woods												;row 6
	hills (river-out hills) hills hills									;row 6
	forest (small-lake forest) (small-lake forest) forest				;row 6
	forest forest forest coast											;row 6
	mountain mountain woods woods										;row 7
	open-ruin (river-out hills) hills hills								;row 7
	forest mountain coast												;row 7
	coast coast coast													;row 7
	nil nil a-fairy		;extra screens in original game
	coast (big-lake forest) (big-lake forest) river river river ;added screens
	coast coast coast coast grave grave desert));added screens (screens I added)

(defun base-scr-biomep (base-scr-num biome)
  (let ((base-scr-biome (aref *base-scr-biomes* (abs base-scr-num))))
	(if (listp base-scr-biome)
	  (member biome base-scr-biome)
	  (eq biome base-scr-biome))))

;Need to update these to take advantage of addition of water sides
(defun type-to-adapt (typ)
  (cond ((eql typ 25) #((0 0) (1 1) nil nil (249 249) nil nil nil nil (166 1)))
		((> typ 20) (aref *s-coast-adapts* (- typ 20)))
		((> typ 15) (aref *n-coast-adapts* (- typ 15)))
		((> typ 10) (aref *forest-adapts* (- typ 10)))
		(t (aref *mountain-adapts* typ))))

(defun adapt-columns (base-scr side new-type)
  (let ((adj-side (if (< base-scr 0)
					(if (eq side 'left) 'right 'left)
					side)))
	(aref
	  (type-to-adapt  (cadr (aref 
							  (if (eq adj-side 'left) *left-type* *right-type*)
							  (abs base-scr))))
	  (cond ((> new-type 20) (- new-type 20))
			((> new-type 15) (- new-type 15))
			((> new-type 9) (- new-type 10))
			(t new-type)))))

(defparameter *mountain-side* #(() (5) (5 6 7 8) (3 4 5 6 7) (2 3 4 5 6 7 8)
								   (tree 2 5 6 7 8) (angle 2 3 4 5 6 7 8) 
								   (angle 3 4 5 6 7) (angle 1 2 3 4 5 6 7 8 9)
								   (4 5 6)))

(defparameter *mountain-vert* #(() (3) (4) (5) (7) (10) (13) (4 13) (6 13)))

(defparameter *extends* '((0 (< 6) 1) (3 (> 11) 1) (11 (< 4) 1) 
						  (52 (< 3) 81) (68 (> 13) 23) (132 (> 9) 55)
						  (133 (> 9) 81) (-132 (< 6) 55) (-133 (< 6) 81)))

;The following array of arrays contains the columns that will replace
;the 2 columns closest to the relevant edge when adapting the various
;screen types to the various edge types. The screen types are commented
;into it on the relevant lines, while the edge types are enumerated in
;*mountain-side*, as which column numbers are open and any special features.
(defparameter *mountain-adapts*
  #(nil
	#((0 0) (1 1) (161 166) (169 166) (2 166) (102 2) (168 166) (240 166) 
			  (163 168) (241 240)) ;1-wide; mostly in (5) category
	#((0 230) (167 160) (6 6) (169 200) (2 200) (102 2) (168 200) (240 160)
			  (163 200) (241 199)) ;low 4-wide; mostly in (5 6 7 8) category
	#((0 169) (167 169) (161 132) (169 168) (2 168) (102 2) (168 2) (240 241)
			  (163 168) (241 240)) ;5-wide; only screen 4-5 transition
	#((0 169) (167 169) (161 132) (169 2) (2 2) (102 2) (168 169) (240 241)
			  (163 2) (241 169)) ;7-wide; for trees, (2 3 4 5 6 7 8) groups
	#((0 241) (167 241) (161 132) (169 168) (2 168) (102 2) (168 241) (240 241) 
			  (163 168) (241 169)) ;angle 5-wide; 48-49 transition
	#((0 200) (167 200) (161 132) (169 200) (2 200) (102 2) (168 200) (240 241)
			  (163 165) (241 169)) ;angle 9-wide; notable only at 116 left side
	#((0 229) (1 229) (161 229) (169 229) (2 229) (102 229) (168 229) (240 229)
			  (163 229) (241 229)) ;special case for screen 17 since it has a
								   ;tree 2 columns from the right edge
	#((0 167) (1 167) (6 161) (169 168) (2 240) (102 2) (168 240) (240 241)
			  (163 168) (241 240)))) ;special case for screen 107 since it has
									;an oddball column direction three from edge

(defparameter *ladder-replacements* '((80 1) (56 6) (181 2) (182 2) (131 6)))
(defparameter *double-ladder* 17)
(defparameter *double-open* 24)
(defparameter *dbl-replacements* '((top 181) (bottom 182) (both 2)))
(defparameter *s-forest-exit* 118)
(defparameter *n-forest-exit* 35)

;Standard ladder columns:
;	double-ladder: 17 [2 high each side]
;	bot low 4-gap: 56 [2 high bot]
;	bot 1-wide gap: 80 [5 high bot]
;	top low 4-gap: 131 [5 high top]
;	top 1-wide gap: 179 [5 high top]
;	bot 7-wide gap: 181 [2 high bot]
;	top 7-wide gap: 182 [2 high top]
;
;Special ladder columns:
;	two-eye ruin: 104, 135
;	coastal cliff cave (see screen 13): 150
;	southern water: 184
;	one-eye ruin: 215
;	ladder-open: not default (added by alter-columns)
(defparameter *ladder-neighbors*
  '((0 1 80 129 166 167 179) ;1-wide gap
	(0 1 4 6 56 129 131 160 161 166 167 179 230 231) ;low 4-gap
	() ;two-eye ruin
	(128) ;coastal cliff cave
	(tl-rock 66 71 81 88 tr-tree tr-rock 97 tl-tree n-rock-water-wall 133 145 
			 151 184 195 246 tl-rock2 tr-rock2 water n-tree-water-wall bl-water 
			 br-water)
	;southern water
	() ;one-eye ruin
	(ladder-river high-river 115 176 212 river-bend) ;river
	() ;default: 7-wide gap
	(247 98 54 55 72 82 water 134 146 s-rock-water-wall s-tree-water-wall
	 tl-water tr-water) ;northern water
	(bl-rock bl-tree bl-open) ;northern water l-corner
	(br-rock br-tree br-open))) ;northern water r-corner

(defparameter *ladder-col-sets* ;order is always bottom then top
  '((80 179) (56 131) (104) (150) (nil 184) (215) (176 ladder-river) (181 182)
			 (82 nil) (bl-open nil) (br-open nil)))

(defparameter *ladder-swaps*
  '((80 (167 1)) (179 (167 1))
	(56 (162 2)) (131 (161 6)) 
	(181 (162 2) (100 2) (216 243) (229 2) (242 102)) 
	(182 (162 2) (165 2) (216 243) (229 2) (242 102))
	(184 (71 81) (133 81) (151 81))))

(defun flip-vert (vert)
  (if (eq vert 'bot) 'top 'bot))

(defun has-ladder (col-num vert)
  (eql 10 (nth (if (eq vert 'bot) 10 0) (column-by-number col-num))))

(defun pick-ladder-col (left col right vert)
  (if (has-ladder col vert)
	'(nil nil nil)
	(if (has-ladder col (flip-vert vert))
	  (pick-double-ladders left col right)
	  (pick-normal-ladder-col left col right vert))))

(defun pick-normal-ladder-col (left col right vert)
  (let* ((category (position-if 
					(lambda (x) (and (member left x) (member right x)))
					*ladder-neighbors*))
		 (ladder-col (nth (if (eq vert 'bot) 0 1) 
						  (nth (if category category 7) *ladder-col-sets*)))
		 (left-col (cadr (assoc left (cdr (assoc ladder-col *ladder-swaps*)))))
		 (right-col (invert-column 
					  (cadr (assoc (invert-column right)
								   (cdr (assoc ladder-col *ladder-swaps*)))))))
	`(,(if left-col left-col nil) ,ladder-col ,(if right-col right-col nil))))

(defun add-ladder (screen base-scr col-num vert)
  	(mapc (lambda (x y) (when x (setf (aref screen y) x)))
		  (pick-ladder-col (aref screen (1- col-num))
						   (aref screen col-num)
						   (aref screen (1+ col-num))
						   vert)
		  `(,(1- col-num) ,col-num ,(1+ col-num))))

(defun remove-ladder (screen base-scr col-num vert)
  (let ((subs '(1 6 nil nil 71 nil 115 2)))
	(setf (aref screen col-num)
		  (if (eql (aref screen col-num) *double-ladder*)
			(if (eql vert 'bot)
			  182
			  181)
			(nth (position-if (lambda (x) (member (aref screen col-num) x))
							  *ladder-col-sets*)
				 subs)))))

(defun pick-double-ladders (left col right)
  `(,(if (eql left 162) 2 left) ,*double-ladder* ,(if (eql right 163) 2 right)))


;relevant "rock-open" columns: 4 7 8 211 213 226 227

(defun add-vert-open (screen base-scr col-num vert)
  (let* ((top-opens `(8 148 197 198 ,*double-open* 213 227 25 35 116 88 
					  ladder-river 147 195 246))
		 (bot-opens `(4 7 148 196 197 208 ,*double-open* 211 226 32 118 87 82
					  176 bl-open br-open 130 134 136 146 209 dock-open))
		 (top-types `((default)
					  (130 148)
					  (196 197)
					  (249 198)
					  ,(append bot-opens `(,*double-open*))
					  (169 213 230)
					  (162 227 232)
					  (22 25 41)
					  (23 35 37 38 39 40 49 73 117 244)
					  (20 21 48 116)
					  (66 71 81 88 97 133 145 151 184)
					  (176 ladder-river high-river 212 115)
					  (147)
					  (195)
					  (246)))
		 (bot-types `((0 1 4 6 129 160 161 166 167)
					  (default)
					  (147 148)
					  (196 249)
					  (197 198)
					  (208)
					  ,(append top-opens `(,*double-open*))
					  (133 210 211)
					  (144 169 226)
					  (22 32 244)
					  (23 37 38 39 40 41 49 73 117 118)
					  (20 21 48 87)
					  (54 55 72 82 98 247)
					  (high-river 115 176)
					  (bl-rock bl-tree bl-open)
					  (br-rock br-tree br-open)
					  (130)
					  (134)
					  (136)
					  (146)
					  (209)
					  (dock-wall dock-open)))
		 (opens (if (eq vert 'bot) bot-opens top-opens))
		 (types (if (eq vert 'bot) bot-types top-types))
		 (default (if (eq vert 'bot) 7 8))
		 (pos (position-if (lambda (x) (member (aref screen col-num) x))
						   types)))
	(setf (aref screen col-num)
		  (if pos (nth pos opens) default))))

(defun treesp (base-scr)
  (or (base-scr-biomep base-scr 'forest)
	  (base-scr-biomep base-scr 'woods)))

(defun coast-lakep (base-scr)
  (or (base-scr-biomep base-scr 'coast)
	  (base-scr-biomep base-scr 'big-lake)
	  (base-scr-biomep base-scr 'small-lake)))

(defconstant +base-scr-coasts+
			 '((13 bot) (28 right bot) (29 left) (37 right bot) (38 left right)
			   (39 left bot) (44 top right) (45 left bot) (51 right bot) 
			   (52 left right) (53 top left) (55 top bot) (60 top right) 
			   (61 left bot) (64 top bot) (66 right bot) (67 left right) 
			   (68 top left) (75 top bot) (80 top right) (81 left right)
			   (82 top left) (85 right bot) (86 left bot) (100 top right) 
			   (101 top left) (106 top bot) (117 right) (118 left right) 
			   (119 left right) (120 top left) (124 left right) 
			   (125 left right) (126 top left) (127 top bot) (129 right) 
			   (130 top left) (131 top right) (132 top) (133 bot)))

(defun direction-flipper (direction)
  (cond ((eq direction 'left) 'right)
		((eq direction 'right) 'left)
		(t direction)))

(defun list-direction-flipper (direction-list)
  (mapcar #'direction-flipper direction-list))

(defun get-base-directions (base-scr)
  (funcall (if (< base-scr 0) #'list-direction-flipper #'identity)
		   (cdr (assoc (abs base-scr) +base-scr-coasts+ :test #'eql))))

(defun connected-coast (left-base right-base)
  (and (member 'right (get-base-directions left-base))
	   (member 'left (get-base-directions right-base))))

(defun remove-vert-open (screen base-scr col-num vert)
  (let ((top-repls 
		  '((8 2) (213 169) (227 232) (25 22) (35 23) (116 48)))
		(bot-repls
		  '((4 6) (7 2) (32 22) (211 210) (226 144) (87 48) (118 23) 
			(176 115) (ladder-open 182) (dock-open dock-wall)))
		(top-water-repls
		  '((88 71 97) (195 71 97) (246 71 97)))
		(bot-water-repls
		  '((bl-open bl-rock bl-tree) (br-open br-rock br-tree) (82 54 72)
									  (134 54 72) (146 54 72)))
		(double-repls
		  `((,*double-open* (7 118) (8 35) (2 23)) 
			(197 (196 nil) (198 nil) (249 nil))))
		(col (aref screen col-num)))
	(setf (aref screen col-num)
		  (cond ((member col (mapcar #'car top-repls))
				 (let ((sub (assoc col top-repls)))
				   (if (and sub (eq vert 'top)) (cadr sub) col)))
				((member col (mapcar #'car bot-repls))
				 (let ((sub (assoc col bot-repls)))
				   (if (and sub (eq vert 'bot)) (cadr sub) col)))
				((member col (mapcar #'car top-water-repls))
				 (let ((sub (assoc col top-water-repls)))
				   (if (eq vert 'top)
					 (if (treesp base-scr)
					   (caddr sub) (cadr sub))
					 col)))
				((member col (mapcar #'car bot-water-repls))
				 (let ((sub (assoc col bot-water-repls)))
				   (if (eq vert 'bot)
					 (if (treesp base-scr)
					   (caddr sub) (cadr sub))
					 col)))
				((member col (mapcar #'car double-repls))
				 (funcall (if (treesp base-scr) #'cadr #'car)
						  (nth (position vert '(top bot both))
							   (cdr (assoc col double-repls)))))
				(t col)))))


(defconstant +pair-tiles+ '((#x8 #x9) (#x15 #x16) (#x17 #x18) (#x1C #x1E)
							(#x1F #x20) (#x21 #x23) (#x24 #x25) (#x2E #x2F)
							(#x30 #x31) (#x32 #x33) (#x34 #x35)))

(defun invert-equal (tile1 tile2)
  (or (eql tile1 tile2)
	  (some (lambda (x) (and (member tile1 x) (member tile2 x))) +pair-tiles+)))

(defun col-invert-equal (col1 col2)
  (every #'identity (mapcar #'invert-equal col1 col2)))

(defun slant-equal (tile1 tile2)
  (or (eql tile1 tile2)
	  (and (member tile1 '(50 51)) (member tile2 '(50 51)))
	  (and (member tile1 '(52 53)) (member tile2 '(52 53)))))
	  
(defun col-slant-equal (col1 col2)
  (every #'identity (mapcar #'slant-equal col1 col2)))

(defun find-opposite-columns (columns)
  (let ((col-nos (loop for x from 0 to (1- (length columns)) collect x)))
	(coerce
	  (mapcar (lambda (x y) (if x (rev-convert-col-num x)
								  (rev-convert-col-num y)))
			  (mapcar (lambda (x)
						(find-if (lambda (y) (and (not (eql x y)) 
									  (col-invert-equal (aref columns x) 
													    (aref columns y))))
								 col-nos))
					  col-nos)
			  col-nos) 'array)))

(defparameter *opposite-columns* (find-opposite-columns *columns*))

(defun opp-column-by-number (col-no) 
  (when col-no
	(aref *opposite-columns* (convert-column-number col-no))))

(defparameter *forest-side* '(() (2 8) (5 6 7) (2 4 6 8) (2 3 4 5 6 7 8)
								 (3 4 5 6 7)))
(defparameter *forest-adapts*
  #(#((21 21) (48 48) (22 22) (38 23) (23 23) (40 40) (244 244)) ;wall (no gap)
	#((21 21) (48 48) (22 23) (38 23) (23 23) (40 23) (244 23)) ;2 gaps
	#((21 21) (48 23) (22 22) (38 23) (23 23) (40 40) (244 244));center 3-wide
	#((21 21) (48 23) (22 23) (38 38) (23 23) (40 23) (244 244)) ;4 gaps
	#((21 21) (48 23) (22 22) (38 23) (23 23) (40 40) (244 244)) ;7-wide gap
	#((21 21) (48 23) (22 22) (38 23) (23 23) (40 40) (244 244))));low 4-gap 

(defparameter *n-coast-side* '(() (4 5) (angle 3 4 5) (2 3 4 5)))

(defparameter *n-coast-adapts*
  #(#((n-rock-water-wall n-rock-water-wall) (81 81) (145 81) (71 71)) ;wall
	#((n-rock-water-wall 81) (81 81) (145 81) (71 71)) ;strict rock
	#((n-rock-water-wall 151) (81 151) (145 151) (71 71)) ;angle strict rock
	#((n-rock-water-wall 151) (81 151) (145 151) (71 71)) ;loose rock
	#((n-tree-water-wall 97) (66 66) (66 66) (97 97)) ;loose tree
	#((n-tree-water-wall 66) (66 66) (66 66) (97 97)))) ;strict tree

(defparameter *s-coast-side* '(() (5 6 7 8) (5) (5 6)))

(defparameter *s-coast-adapts*
  #(#((s-rock-water-wall s-rock-water-wall) (54 54) (55 55) (98 98)) ;wall
	#((s-rock-water-wall s-rock-water-wall) (54 54) (55 55) (98 98)) ;loose rock
	#((s-rock-water-wall s-rock-water-wall) (54 54) (55 55) (98 98));strict rock
	#((s-tree-water-wall s-tree-water-wall) (72 72) (55 55) (98 98)) ;loose tree
	#((s-tree-water-wall s-tree-water-wall) (72 72) (55 55) (98 98))))
																;strict tree

(defconstant +top-forest-subs+ '((48 . 116) (22 . 25) (118 . 24) (87 . 24) 
											(26 . 24)))
(defconstant +bot-forest-subs+ '((48 . 87) (22 . 26) (35 . 24) (116 . 24) 
										   (25 . 24)))

(defun forest-is-open (col top-bot)
  (flet ((forest-open (top-bot)
					 (let ((tile (if (eq top-bot 'top) 0 10)))
					   (eql (nth col tile) 14))))
	(cond ((eq top-bot 'both) (and (forest-open 'top) (forest-open 'bot)))
		  ((eq top-bot 'none) (not (or (forest-open 'top) (forest-open 'bot))))
		   (t (forest-open top-bot)))))

(defun forest-vert (col top-bot)
  (let ((repl col))
	(cond ((eq top-bot 'none)
			(when (forest-is-open (repl 'top))
			  (setf repl (car (rassoc repl +top-forest-subs+))))
			(if (forest-is-open (repl 'bot))
			  (car (rassoc repl +bot-forest-subs+))
			  repl))
		  ((eq top-bot 'top)
			(if (forest-is-open (repl 'top))
			  repl
			  (cdr (assoc repl +top-forest-subs+))))
		  ((eq top-bot 'bot)
			(if (forest-is-open (repl 'bot))
			  repl
			  (cdr (assoc repl +bot-forest-subs+))))
		  ((eq top-bot 'not-top)
			(if (forest-is-open (repl 'top))
			  (car (rassoc repl +top-forest-subs+))
			  repl))
		  ((eq top-bot 'not-bot)
			(if (forest-is-open (repl 'bot))
			  (car (rassoc repl +bot-forest-subs+))
			  repl))
		  (t 24))))

(defun add-forest-open (scr base-scr col-num vert)
  (setf (aref scr col-num) (forest-vert (aref scr col-num) vert)))

(defun remove-forest-open (scr base-scr col-num vert)
  (setf (aref scr col-num) (forest-vert (aref scr col-num)
										(if (eq vert 'top)
										  'not-top
										  'not-bot))))

(defstruct scr-info num biome left right top bot)


(defun extend-screen (scr base-scr extends col)
  (let ((extend (assoc base-scr extends)))
	(flet ((base-col (col-num)
					 (nth (if (< base-scr 0) (- 15 col) col)
						  (aref *name-screens* (abs base-scr)))))
	(when (and extend (funcall (caadr extend) col (cadadr extend)))
	  (mapc (lambda (x) 
			  (when (or (eql (aref scr x) (base-col x))
						(eql (aref scr x)
							 (simplify-column (base-col x)))))
				(setf (aref scr x) (caddr extend)))
			(interval col (cadadr extend)))))))

(defun adapt-left (scr base-scr edge-type)
  (unless (eql edge-type (left-type base-scr))
	(mapc (lambda (pos col) (setf (aref scr pos) col))
		  '(0 1)
		  (adapt-columns base-scr 'left edge-type))))

(defun adapt-right (scr base-scr edge-type)
  (unless (eql edge-type (right-type base-scr))
	(mapc (lambda (pos col) (setf (aref scr pos) (invert-column col)))
		  '(15 14)
		  (adapt-columns base-scr 'right edge-type))))

;The two options for edge-type are: 
;	4 (for center 7-wide opens)
;	9 (for center 3-wide opens)
;	0 (for walls)
;	any that need no adaptation to different biomes
(defun adapt-to-special (scr base-scr edge-type side)
  (unless (zerop edge-type)
	(extend-screen scr base-scr *extends* (if (eq side 'left) 0 15)))
  (funcall (if (eq side 'left) #'adapt-left #'adapt-right)
		   scr base-scr
		   (if (and (> (funcall (if (eq side 'left)
								  #'left-adapt-type #'right-adapt-type)
								base-scr) 9)
					(< (funcall (if (eq side 'left)
								  #'left-adapt-type #'right-adapt-type)
								base-scr) 16)
					(not (zerop edge-type)))
			 (cond ((eql edge-type 4) 14)
				   ((eql edge-type 2) 19)
				   (t 12))
			 edge-type)))

;Finds the type of the screen's left edge. Edges of special screens or
;edges with water in them return nil.
;Added additional functionality; a negative number represents the base screen
;was inverted, so left-type gives its usual right-type and vice versa
(defun left-type (base-scr)
  (car (aref (if (> base-scr -1) *left-type* *right-type*) (abs base-scr))))

;Operates as left-type, but gets the screen's right edge
(defun right-type (base-scr)
  (car (aref (if (> base-scr -1) *right-type* *left-type*) (abs base-scr))))

(defun left-adapt-type (base-scr)
  (cadr (aref (if (> base-scr -1) *left-type* *right-type*) (abs base-scr))))

(defun right-adapt-type (base-scr)
  (cadr (aref (if (> base-scr -1) *right-type* *left-type*) (abs base-scr))))

;left and right are screen numbers (1-136); output is either 0 or 1 for
;which screen should be adapted to (0 for left, 1 for right)
(defun pick-open-side (left right)
  (let ((options (remove-if-not #'identity 
					(list (when (not (zerop (right-type left))) 0)
						  (when (not (zerop (left-type right))) 1)))))
	(if options
	  (nth (mrandom (length options)) options)
	  -1)))

(defun adapt-wall (base-left base-right left right)
  (adapt-right left base-left 0)
  (adapt-left right base-right 0))

(defun normal-adapt-side (base-left base-right left right has-edge extends)
  (if has-edge
	(let* ((pick (pick-open-side base-left base-right))
		   (edge-type (cond ((zerop pick)
							 (right-type base-left))
							((eql pick -1)
							 1) ;should probably change this later; default
							(t (left-type base-right)))))
	  (extend-screen left base-left extends 15)
	  (extend-screen right base-right extends 0)
	  (adapt-left right base-right edge-type)
	  (adapt-right left base-left edge-type))
	(adapt-wall base-left base-right left right)))

;Only applies to mountain/hill crossed with forest/woods.
;If mountain is crossed with hills or forest is crossed with woods, should
;just use normal-adapt-side; need special options for other biomes
(defun cross-biome-adapt-side (base-left base-right left right has-edge extends)
  (if has-edge
	(let* ((typ (mrandom 2))
		   (mountain-opt (nth (mrandom 2) (if (zerop typ) '(7 9) '(4 8))))
		   (forest-opt (if (zerop typ) 12 14)))
	  (extend-screen left base-left extends 15)
	  (extend-screen right base-right extends 0)
	  (adapt-left right base-right (if 
									 (or
									   (and (> (left-type base-right) 0)
											(< (left-type base-right) 10))
									   (funcall
										 (if (listp (aref *base-scr-biomes*
														  (abs base-right)))
										   #'intersection #'member)
										 (aref *base-scr-biomes* 
											   (abs base-right))
										 '(mountain hills coast river-in)))
									 mountain-opt
									 forest-opt))
	  (adapt-right left base-left (if 
									(or
									  (and (> (right-type base-left) 0)
										   (< (right-type base-left) 10))
									  (funcall
										(if (listp (aref *base-scr-biomes* 
														 (abs base-left)))
										  #'intersection #'member)
										(aref *base-scr-biomes* (abs base-left))
										'(mountain hills coast river-in)))
									mountain-opt
									forest-opt)))
	(adapt-wall base-left base-right left right)))


(defun adapt-side (base-left base-right left right has-edge extends)
  (if (or (eq (null (if (zerop (right-type base-left))
					  (treesp base-left)
					  (> (right-type base-left) 10)))
			  (null (if (zerop (left-type base-right))
					  (treesp base-right)
					  (> (left-type base-right) 10))))
		  (connected-coast base-left base-right))
	(normal-adapt-side base-left base-right left right has-edge extends)
	(cross-biome-adapt-side base-left base-right left right has-edge extends)))

(defconstant +tiles-not-to-replace+ 
			 '(#x27 #x28 #x0C #x13 #x26 #x29 #x2A #x2B #x2C #x1C #x1F 
			   #x1E #x20))

(defconstant +base-cols-not-to-replace+
	(append '(3 5 33 34 36 180 208 228 18 water 68 tl-water tr-water bl-water 
			  br-water tl-rock tl-rock2 tr-rock tr-rock2)
			(find-columns-that 
			  (lambda (x) (intersection x +tiles-not-to-replace+)))))

(defconstant +bad-neighbors+
			 (find-columns-that (lambda (x) (intersection x '(#x0c #x27)))))

(defparameter *cols-not-to-replace* +base-cols-not-to-replace+)

(defun valid-neighbors (screen col)
  (not (or (zerop col)
		   (eql col (1- (length screen)))
		   (intersection (list (aref screen (1- col)) (aref screen (1+ col)))
						 +bad-neighbors+))))

(defun add-col-not-to-replace (col-num)
  (push col-num *cols-not-to-replace*))

(defun find-vert-edge-tile-cols (screen vert tile)
  (let ((pos -1)
		(side (if (eq vert 'bot)
				(lambda (x) (nth 10 x))
				(lambda (x) (nth 0 x)))))
	(flet ((filter-valid-pos (col-num) 
							 (incf pos) 
							 (if (eql tile (funcall side 
													(column-by-number col-num)))
							   pos 
							   nil)))
	  (remove-if-not #'identity (mapcar #'filter-valid-pos (coerce screen 'list))))))

(defun find-ladder-cols (screen vert)
  (find-vert-edge-tile-cols screen vert #x0A))

(defun find-open-cols (screen vert)
  (find-vert-edge-tile-cols screen vert #x0E))

(defun find-vert-exits (screen vert)
  (append (find-vert-edge-tile-cols screen vert #x0E)
		  (find-vert-edge-tile-cols screen vert #x0A)))

(defun remove-edge (scr base-scr col-num vert)
  (if (eql #x0A (nth (if (eq vert 'bot) 10 0) 
					 (column-by-number (aref scr col-num))))
	(remove-ladder scr base-scr col-num vert)
	(remove-vert-open scr base-scr col-num vert)))

(defun ladder-adapt (scr1 base-scr1 scr2 base-scr2 extends)
  (vert-adapt scr1 base-scr1 scr2 base-scr2 extends 
			  #'find-ladder-cols #'add-ladder (append *cols-not-to-replace*
													  +open-vert-cols+)))

(defun open-vert-adapt (scr1 base-scr1 scr2 base-scr2 extends)
  (vert-adapt scr1 base-scr1 scr2 base-scr2 extends
			  #'find-open-cols #'add-vert-open (append *cols-not-to-replace*
													   +ladder-cols+)))

;finds one vert edge type of a screen
;ARGS: 
;	scr: the screen whose edge type must be determined; 
;			it is an array of column numbers
;	base-scr: the number of its original screen (a number from 0 to 123)
;	edges: the column numbers on which the given edge is open (either from
;			having a ladder or an open space)
;	vert: either top or bot; the vertical side being checked
;Outputs:
;	trees: whether or not there is an opening, the screen is a forest or woods
;			screen and thus cannot have ladders
;	ladder: the screen's edge has ladders
;	rock-open: the screen is not a forest screen, but its edge has empty tiles
;	nil: the screen is not a trees screen and its edge is solid wall
(defun find-vert-edge-type (scr base-scr edges vert)
  (if (treesp base-scr) 
	'trees
	(if edges
	  (if (eql (nth (if (eq vert 'bot) 10 0) (column-by-number 
						 (aref scr (car edges)))) #x0A)
							'ladder
							'rock-open)
	  nil)))

  
(defun vert-adapt-type (scr1 base-scr1 edges1 scr2 base-scr2 edges2)
  (let ((edge-type1 (find-vert-edge-type scr1 base-scr1 edges1 'bot))
		(edge-type2 (find-vert-edge-type scr2 base-scr2 edges2 'top)))
	(cond ((member 208 (coerce scr1 'list)) 'open)
		  ((member 'trees (list edge-type1 edge-type2)) 'open)
		  ((intersection (append (coerce scr1 'list) (coerce scr2 'list))
						 +desert-columns+) 'open)
		  ((not (or edge-type1 edge-type2)) (nth (mrandom 2) '(ladder open)))
		  ((and edge-type1 edge-type2 (not (eq edge-type1 edge-type2)))
				(nth (mrandom 2) '(ladder open)))
		  ((member 'ladder (list edge-type1 edge-type2)) 'ladder)
		  (t 'open))))

(defun remove-vert-edges-by-tile (scr base-scr edges vert tile)
  (let ((vert-num (if (eq vert 'bot) 10 0)))
	(labels ((recursive-remove (in-lst out-lst)
				(if in-lst
				  (if (eql tile (nth vert-num (column-by-number 
												(aref scr (car in-lst)))))
					(progn (remove-edge scr base-scr (car in-lst) vert)
						   (recursive-remove (cdr in-lst) out-lst))
					(recursive-remove (cdr in-lst) (cons (car in-lst) out-lst)))
				  (reverse out-lst))))
	  (recursive-remove edges nil))))


(defun adapt-vert (base-scr1 base-scr2 scr1 scr2 extends)
  (mixed-vert-adapt scr1 base-scr1 scr2 base-scr2 extends))


;Used in case the two screens being adapted don't have the same exit types;
;if they're different, it removes one of their exits, and its return
;is used by vert-adapt to nil that argument.
(defun mixed-vert-adapt (scr1 base-scr1 scr2 base-scr2 extends)
  (let ((edges1 (find-vert-exits scr1 'bot))
		(edges2 (find-vert-exits scr2 'top)))
	(let* ((edge-type 
			 (vert-adapt-type scr1 base-scr1 edges1 scr2 base-scr2 edges2))
		   (adder (if (eq edge-type 'open) #'add-vert-open #'add-ladder))
		   (tile (if (eq edge-type 'open) #x0A #x0E));tile to remove
		   (do-not-replace (if (eq edge-type 'open)
							 +ladder-cols+ 
							 +open-vert-cols+)))
	  (vert-adapt scr1 base-scr1 
				  (remove-vert-edges-by-tile scr1 base-scr1 edges1 'bot tile)
				  scr2 base-scr2 
				  (remove-vert-edges-by-tile scr2 base-scr2 edges2 'top tile)
				  extends adder 
				  (append *cols-not-to-replace* do-not-replace)))))


(defun edges-valid-p (scr edges do-not-replace)
  (and edges
	   (every (lambda (x) (and (not (member (aref scr x) do-not-replace))
							   (valid-neighbors scr x)))
			  edges)))


;Used to vertically connect two screens that have an edge between them;
;May work on the majority of screens, but currently only prepared to 
;work with the standard mountain/hill/forest/woods screens
(defun vert-adapt (scr1 base-scr1 edges1 scr2 base-scr2 edges2 extends adder
						do-not-replace)
  (flet ((adapt-screen (scr base-scr cur-edges new-edges vert)
						(mapcar (lambda (x) 
								  (unless (member x new-edges)
									(remove-edge scr base-scr x vert)))
								  (interval 0 15))
						(mapcar (lambda (x) 
								  (extend-screen scr base-scr extends x)
								  (funcall adder scr base-scr x vert))
								new-edges)))
	(when (member 'dock-open (mapcar (lambda (x) (aref scr1 x)) edges1))
	  (let ((dock-loc (find-if (lambda (x) (eql 'dock-open (aref scr1 x))) 
							   edges1)))
		(extend-screen scr2 base-scr2 extends dock-loc)
		(funcall adder scr2 base-scr2 dock-loc 'top)
		(push dock-loc edges2)))
	(let ((edges1-valid (edges-valid-p scr2 edges1 do-not-replace))
		  (edges2-valid (edges-valid-p scr1 edges2 do-not-replace)))
	  (cond ((and edges1-valid edges2-valid)
				(if (eql (mrandom 2) 0)
				  (adapt-screen scr2 base-scr2 edges2 edges1 'top)
				  (adapt-screen scr1 base-scr1 edges1 edges2 'bot)))
			(edges1-valid 
			  (adapt-screen scr2 base-scr2 edges2 edges1 'top))
			(edges2-valid 
			  (adapt-screen scr1 base-scr1 edges1 edges2 'bot))
			(t
			  (let ((valid-cols 
					   (remove-if (lambda (x)
									(or
									  (member (aref scr1 x)
											  do-not-replace)
									  (not (valid-neighbors scr1 x))
									  (member (aref scr2 x) 
											  do-not-replace)
									  (not (valid-neighbors scr2 x))))
								  (interval 2 13))))
				(if valid-cols
				  (let ((col-choice (nth (mrandom (length valid-cols)) 
									   valid-cols)))
				 (adapt-screen scr1 base-scr1 edges1 `(,col-choice) 'bot)
				 (adapt-screen scr2 base-scr2 edges2 `(,col-choice) 'top))
				  (if (equal do-not-replace *cols-not-to-replace*)
					(progn (format t "No valid edges~&Top base screen: ~d~& Bottom base screen: ~d~&"
							base-scr1 base-scr2)
						   'no-valid-edges)
					(vert-adapt scr1 base-scr1 edges1 scr2 base-scr2 edges2
								extends adder *cols-not-to-replace*)))))))))


