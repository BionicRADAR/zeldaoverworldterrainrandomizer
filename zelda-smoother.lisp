(defun smooth-map (screens base biomes horiz-edges vert-edges)
  (burn-access screens))

(defun get-side-neighbor-tiles (screen tile-coord)
  (mapcar
	(lambda (x) (get-tile-from-screen screen x))
	(remove-if (lambda (x) (or (< (cadr x) 0) (>= (cadr x) (length screen))))
			   `((,(car tile-coord) ,(1- (cadr tile-coord)))
				 (,(car tile-coord) ,(1+ (cadr tile-coord)))))))

(defun burn-access (screens)
  (labels ((open-up (screen col-num)
			 (let ((closed '(22 40 244))
				   (opened '(32)))
			   (let ((changeables
					   (remove-if (lambda (num)
									(or (< num 0)
										(> num (1- (length screen)))
										(not 
										  (member (aref screen num)
												  (append closed opened)))))
								  `(,(1- col-num) ,(1+ col-num)))))
				 (when changeables
				   (let ((to-change (nth (mrandom (length changeables))
										 changeables)))
					 (setf (aref screen to-change)
						   (if (member (aref screen to-change) closed)
							 23 118)))))))
		   (is-accessible (screen col-num)
			 (some #'passable (get-side-neighbor-tiles screen `(2 ,col-num)))))
	(mapc (lambda (screen)
			(unless (numberp screen)
			  (mapc (lambda (col-num)
					  (when (and (eql (aref screen col-num) 20)
								 (not (is-accessible screen col-num)))
						(open-up screen col-num)))
					(interval 0 (1- (length screen))))))
		  (mapcar (lambda (screen-coord) (get-arr2d screens screen-coord))
				  (get-coords-list (array-dimension screens 1)
								   (array-dimension screens 0))))))

(defconstant +cave-replacements+
			 '((16 . 2) (133 . 71) (210 . 2) (211 . 7)))

(defun remove-cave-from-screen (screen)
  (mapc (lambda (x)
		  (let ((repl (cdr (assoc (aref screen x) +cave-replacements+))))
			(when repl (setf (aref screens x) repl))))
		(interval 0 (1- (length screen)))))

(defun find-open-tile-in (screen coords)
  (find-if (lambda (coord) (passable (get-tile-from-screen screen coord)))
		   coords))
