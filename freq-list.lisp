;freq-list.lisp
;defines functions for interacting with "frequency lists" (as I have defined
;them), which are lists whose elements are two-element lists; the first element
;is the actual item in the list, and the second is its frequency
;(how many times it's in the list).
;A frequency list looks like: ((val0 freq0) (val1 freq1) (val2 freq2)...)
;I made these essentially so I could do random drawing without replacement,
;using pop-nth-in-freq-list


;Determines the number of items in the freq-list, which is the sum of
;all the item counts. (To get the standard "list" length, simply use
;(length).
(defun freq-list-length (freq-list)
  (labels ((acc-freq-list (lst acc)
				(if lst
				  (acc-freq-list (cdr lst) (+ acc (cadar lst)))
				  acc)))
	(acc-freq-list freq-list 0)))


;Gives the nth item in the freq-list, indexed from 0;
;This differs from the standard nth in that each item counts
;the number of times in its frequency count; if an item has 0
;in its frequency count, it actually doesn't count at all.
(defun find-nth-in-freq-list (n freq-list)
  (let ((new-n (- n (cadar freq-list))))
	(if (< new-n 0)
	  (caar freq-list)
	  (find-nth-in-freq-list new-n (cdr freq-list)))))




;Pops off the front element from the freq-list, either decreasing its count
;or, if its count reaches zero, removing the car from the list.
(defmacro freq-list-pop (freq-list)
  (multiple-value-bind (dummies vals new setter getter)
	(get-setf-expansion freq-list)
	(let ((fun-name (gensym)))
	  (when (cdr new) (error "Can't expand this."))
	  `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
		 (labels ((,fun-name (lst) 
							(if (and lst (< (cadar lst) 1))
							  (,fun-name (cdr lst))
							  lst)))
		   (setq ,(car new) (,fun-name ,(car new)))
		   (prog1 (caar ,(car new))
			 (when ,(car new) 
			   (decf (cadar ,(car new)))
			   (when (< (cadar ,(car new)) 1)
				 (setq ,(car new) (,fun-name ,(car new)))))
			 ,setter))))))



;Acts as the push function for freq-lists.
;If the item is already in the list, it increments its count
;Otherwise, it adds the item to the list with a count of 1
(defmacro freq-list-push (item freq-list)
  (multiple-value-bind (dummies vals new setter getter)
	(get-setf-expansion freq-list)
	(let ((item-name (gensym))
		  (one-name (gensym)))
	  (when (cdr new) (error "Can't expand this."))
	  `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter)
				(,item-name ,item) (,one-name 1))
		 (if (assoc ,item-name ,(car new))
		   (incf (cadr (assoc ,item-name ,(car new))))
		   (setq ,(car new) (cons (list ,item-name ,one-name) ,(car new))))
		 ,setter))))

;Old version of freq-list-push; didn't work on freq-lists that were
;array elements
;(defun freq-list-push (item freq-list)
;  (if (member item (mapcar #'car freq-list))
;	(incf (cadr (assoc item freq-list)))
;	(push `(,item 1) freq-list)))


;This function is sort of like find-nth above, except instead of returning
;the value at that position, it returns the list position number.
;This number could, for example, be used with the nth function
;on the freq list to get the (value frequency) pair.
;Hence, this function finds the "n" for the standard list "nth" function.
(defun find-n-in-freq-list (n freq-list)
  (labels ((find-n (freq-list n acc)
				(let ((new-n (- n (cadar freq-list))))
				  (if (< new-n 0)
					acc
					(find-n (cdr freq-list) new-n (1+ acc))))))
	(find-n freq-list n 0)))


;This function finds the nth value in the frequency list (nth indexed from 0
;using the frequency counts, so an item counts as many times as its frequency)
;returns that value and decrements its frequency. 
(defun pop-nth-in-freq-list (n freq-list)
  (let ((lst-n (find-n-in-freq-list n freq-list)))
	(decf (cadr (nth lst-n freq-list)))
	(car (nth lst-n freq-list))))
