;;palettes.lisp
;;Part of the zelda overworld randomizer
;;Handles finding the palette for each screen on the map
;;Result is a 2D array, each containing a list with two elements,
;;one for inner palette and the other for outer.
;;@author Nathaniel Schleicher

(defconstant +special-inner-palette-screens+
			 '((30 . green) (39 . green) (64 . green) (79 . green) 
							(96 . brown) (97 . brown)))

(defparameter *palettes* (make-array '(8 16)))

(defconstant +standard-biome-palettes+
			 '((mountain . brown) (woods . brown) (forest . green)
			   (hills . green) (desert . brown) (graveyard . gray)
			   (coast . brown)))

(defconstant +backup-palettes+
			 '((pond . brown) (open-ruin . brown) (spring . brown)
			   (river-in . brown)))

(defun init-palettes ()
  (setf *palettes* (make-array '(8 16))))

(defun pick-outer-palette (biome base-scr)
  (let ((stand (cdr (assoc (find-if (lambda (x) 
									  (assoc x +standard-biome-palettes+))
									biome)
						   +standard-biome-palettes+)))
		(backup (cdr (assoc (find-if (lambda (x) 
									  (assoc x +backup-palettes+))
									biome)
						   +backup-palettes+))))
	(cond ((member 'a-fairy biome) 'brown)
		  ((member 'island biome) (if (member 'coast biome) 'gray 'brown))
		  (stand stand)
		  (backup backup)
		  (t (princ biome) (fresh-line) (princ base-scr) (fresh-line) nil))))

(defun pick-inner-palette (biome base-scr)
  (let ((stand (cdr (assoc (find-if (lambda (x) 
									  (assoc x +standard-biome-palettes+))
									biome)
						   +standard-biome-palettes+)))
		(backup (cdr (assoc (find-if (lambda (x) 
									  (assoc x +backup-palettes+))
									biome)
						   +backup-palettes+)))
		(spec (cdr (assoc base-scr +special-inner-palette-screens+))))
	(cond (spec spec)
		  ((member 'a-fairy biome) 'green)
		  ((member 'island biome) 
		   (if (member 'coast biome) 'gray (nth (mrandom 2) '(brown green))))
		  ((member 'open-ruin biome)
		   (nth (mrandom 2) '(brown green)))
		  (stand stand)
		  (backup backup))))

(defun set-palettes (width height palettes biomes base)
  (mapc
	(lambda (coord)
	  (let ((biome (get-arr2d biomes coord))
			(base-scr (get-arr2d base coord)))
		(set-arr2d palettes coord
				   `(,(pick-outer-palette biome base-scr)
					 ,(pick-inner-palette biome base-scr)))))
	(get-coords-list width height)))

(defun get-coords-within (source-set dist)
  (remove-duplicates (apply #'append 
							(mapcar (lambda (x) (enumerate-dist x dist))
									source-set))
					 :test #'equal))

(defun spread-grave (width height palettes biomes grave-coords)
  (let* ((close-opts (remove-if 
					   (lambda (x) (or (not (valid-coordp width height x))
									   (intersection '(graveyard a-fairy) 
													 (get-arr2d biomes x))))
					   (get-coords-within grave-coords 1)))
		 (close-opt (nth (mrandom (length close-opts)) close-opts))
		 (far-opts (remove-if 
					 (lambda (x) (or (not (valid-coordp width height x))
									 (intersection '(graveyard a-fairy) 
												   (get-arr2d biomes x))
									 (equal close-opt x)))
					 (get-coords-within grave-coords 2)))
		 (far-opt1 (nth (mrandom (length far-opts)) far-opts))
		 (far-opt2 (nth (mrandom (1- (length far-opts)))
						(remove far-opt1 far-opts :test #'equal))))
	(set-arr2d palettes close-opt '(gray gray))
	(mapc (lambda (x) 
			(princ x)
			(setf (cdr (get-arr2d palettes x)) '(gray)))
		  `(,far-opt1 ,far-opt2))))

(defun make-palettes (width height biomes base grave-coords)
  (init-palettes)
  (set-palettes width height *palettes* biomes base)
  (spread-grave width height *palettes* biomes grave-coords))
