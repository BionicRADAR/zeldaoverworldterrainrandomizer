(defun combine-right-freqs (right r-two)
  (mapcar 
	(lambda (x) (let ((item-freq (assoc (car x) r-two)))
				  (list (car x)
					(if item-freq
					  (* (1+ (cadr item-freq)) (cadr x))
					  (cadr x)))))
	right))

(defun filter-next-col (cur-lst next-top next-bot biome-info)
  (let ((top-filtered
		  (if next-top
			(remove-if
			  (cond ((eql (car next-top) 'ladder) 
					 (lambda (x) 
					   (not (member (car x) (biome-info-n-ladder biome-info)))))
					((eql (car next-top) 'empty)
					 (lambda (x) 
					   (not (member (car x) (biome-info-n-open biome-info)))))
					((eql (car next-top) 'wall)
					 (lambda (x) 
					   (not (member (car x) (biome-info-n-wall biome-info))))))
			  cur-lst)
			cur-lst)))
	(if next-bot
	  (remove-if
		(cond ((eql (car next-top) 'ladder) 
			   (lambda (x) (not (member (car x) 
										(biome-info-n-ladder biome-info)))))
			  ((eql (car next-top) 'empty)
			   (lambda (x) (not (member (car x) 
										(biome-info-n-open biome-info)))))
			  ((eql (car next-top) 'wall) 
			   (lambda (x) (not (member (car x) 
										(biome-info-n-wall biome-info))))))
		top-filtered)
	  top-filtered)))
	  
(defun generate-normal-screen (biome-info left right top-lst bot-lst)
  (labels ((choose-column (left l-two right top bot)
								   (princ 'choosing-column) (fresh-line)
				(let* ((comb-left (if (or (not l-two) (eql left l-two))
									(aref (biome-info-r-adj biome-info) left)
									(combine-right-freqs 
									  (aref (biome-info-r-adj biome-info) left)
									  (aref (biome-info-r-two biome-info) l-two)
									  )))
				  (adj-list 
					(filter-next-col
						(remove-if 
						  (lambda (x) (not (and (member 
												  (tile-type 
													(car (aref *columns* 
															   (car x)))) 
												  top)
												(member
												  (tile-type
													(nth 10 (aref *columns* 
																  (car x))))
												  bot))))
						  (if right
							(mapcar (lambda (x) (list x
													  (+ (cadr 
														   (assoc x comb-left))
														 (cadr 
														   (assoc x right)))))
									(intersection 
									  (mapcar #'car comb-left)
									  (mapcar #'car right)))
							comb-left)) (cadr top-lst) (cadr bot-lst) 
						biome-info)))
				  (princ 'named-variables) (fresh-line)
				  (princ adj-list) (fresh-line)
				  (if adj-list
					(find-nth-in-freq-list 
					  (mrandom (freq-list-length adj-list)) adj-list)
					nil ;figure out what to do if impossible set of conditions
						;are involved
					)))
		   (choose-leftmost-column (border)
								   (princ 'choosing-leftmost) (fresh-line)
				(let ((options
				  (gethash border (biome-info-l-exit-hash biome-info))))
				  (car (nth (mrandom (length options)) options))))
		   (choose-rightmost-column (left l-two border)
								   (princ 'choosing-rightmost) (fresh-line)
				(if border
					(choose-column left l-two (mapcar (lambda (x) (list x 1))
					  (gethash border (biome-info-r-exit-hash biome-info)))
								   *all-tile-types* *all-tile-types*)
					(choose-column left l-two (mapcar (lambda (x) (list x 1))
											(biome-info-right-edge biome-info))
								   *all-tile-types* *all-tile-types*)
					;Should probably have special backup plan for rightmost
					;column failing, since it's the most likely to fail
					))
		   (fill-middle-screen (left l-two top-lst bot-lst num acc)
								(princ 'choosing-num) (princ num) (fresh-line)
								(princ 'left) (princ left) (fresh-line)
				(if (zerop num) (reverse acc)
				  (let ((next-column (choose-column left l-two nil (car top-lst)
													(car bot-lst))))
					(if next-column
					  (fill-middle-screen 
						next-column left
						(cdr top-lst) (cdr bot-lst) (1- num)
						(cons next-column acc))
					  nil)))))
	(let* ((left-col (choose-leftmost-column left))
		   (middle (fill-middle-screen 
					 left-col nil
					 (cdr top-lst) (cdr bot-lst) 14 nil)))
	  (if middle
		(append (list left-col) middle 
				(list (choose-rightmost-column 
						(car (last middle)) (nth 12 middle)
							  right)))
		nil))))


(defun choose-next-column (col)
  (let* ((hash (aref *adj-columns* col))
		 (r (mrandom (gethash 'total hash))))
	(loop for column being each hash-key of hash
		  using (hash-value freq) 
		  unless (eq column 'total)
		  do (decf r freq)
		  when (< r 0)
		  return column)))

(defun generate-screen (biome)
  (let ((cur-column 0))
	(loop for col below *screenwidth*
	  do (setf cur-column (choose-next-column cur-column))
	  collect (aref *acolumns* cur-column))))

