;This file has functions and a struct for a heap-based min-priority-queue
;The items in the queue should be lists of the form (key data)
;For example, I originally made this queue for running Dijkstra's algorithm
;for a Zelda Randomizer project. The items it puts in the queue will be of 
;the form (distance coordinates), where distance is the key.

;Finds the index of the parent of the item at index i
(defun heap-parent (i) (floor (- i 1) 2))

;Finds the index of the left subchild of the item at index i
(defun heap-left (i) (1- (* 2 (1+ i))))

;Finds the index of the right subchild of the item at index i
(defun heap-right (i) (* 2 (1+ i)))

;The heap structure
;arr: the array containing the heap's data
;len: the heap's length (may not be the length of the array)
(defstruct heap arr len)

;Gets the i'th item in the heap
;heap: the heap that contains the item
;i: the index of the item
(defun heap-get-i (heap i) (when (< i (heap-len heap)) 
							 (aref (heap-arr heap) i)))

;Gets the key of the i'th item in the heap
;heap: the heap that contains the item
;i: the index of the item
(defun heap-get-key (heap i) (car (heap-get-i heap i)))

;Sets the key of the i'th item in the heap
;heap: the heap that contains the item
;i: the index of the item
;new-key: the new key that the item's key is being changed to
(defun heap-set-key (heap i new-key) 
  (setf (car (aref (heap-arr heap) i)) new-key))

;A helper function that takes a list of (datum key) pairs
;and returns the datum whose key is minimal;
;I called it min-index because the data I used were indices, ie
;the list contained (index key) pairs.
;lst: the list containing the datum key pairs
(defun min-index (lst)
  (car (reduce (lambda (x y) (if (< (cadr x) (cadr y)) x y)) lst)))

;Maintains a heap by checking a given item against its children to see
;if it must be swapped with one of them.
;When called on an index, it ensures that the subtree starting at that index
;will properly follow the min-heap property. It assumes everything above it is
;correct.
;heap: the heap to be sorted
;i: the index currently being heapified; its subtree will properly follow
;	the min-heap property when the whole function is done.
(defun min-heapify (heap i)
  (let* ((l (heap-left i)) (r (heap-right i))
		 (smallest (min-index 
					 (append 
					   `((,i ,(heap-get-key heap i)))
					   (when (< l (heap-len heap)) 
						 `((,l ,(heap-get-key heap l))))
					   (when (< r (heap-len heap)) 
						 `((,r ,(heap-get-key heap r))))))))
	(unless (eql smallest i)
	  (setf (aref (heap-arr heap) smallest)
			(prog1 (heap-get-i heap i)
			  (setf (aref (heap-arr heap) i) (heap-get-i heap smallest))))
	  (min-heapify heap smallest))))

;Takes an array, makes and returns a min-heap from it
;arr: the array that will be turned into a heap.
(defun build-min-heap (arr)
  (let ((new-heap (make-heap :arr arr :len (length arr))))
	(loop for i from (floor (length arr) 2) downto 1 do
		  (min-heapify new-heap i))
	new-heap))

;returns the minimum item from the heap (the item at index 0) but does not
;remove it.
;heap: the heap that the minimum belongs to.
(defun heap-minimum (heap) (heap-get-i heap 0))

;returns the minimum item from the heap and removes it, re-heapifying
;where necessary
;heap: the heap from which the minimum is being extracted.
(defun extract-min (heap)
  (if (< (heap-len heap) 1) nil
	(prog1
	  (heap-minimum heap)
	  (setf (aref (heap-arr heap) 0)
			(heap-get-i heap (1- (heap-len heap))))
	  (setf (heap-len heap) (1- (heap-len heap)))
	  (min-heapify heap 0))))

;decreases the key of the item in the heap with the given index, 
;checking its parent to see if it needs to switch it with the item
;in order to maintain the min-heap property
;heap: the heap on which the operation is being performed
;i: the index of the item whose key is being decreased
;key: the new key to which the item's key is being changed.
(defun decrease-key (heap i key)
  (unless (> key (heap-get-key heap i))
	(heap-set-key heap i key)
	(labels ((float-up (i)
				(when (and (> i 0) (> (heap-get-key heap (heap-parent i))
									  (heap-get-key heap i)))
				  (setf (aref (heap-arr heap) (heap-parent i))
						(prog1 (heap-get-i heap i)
						  (setf (aref (heap-arr heap) i) 
								(aref (heap-arr heap) (heap-parent i)))))
				  (float-up (heap-parent i)))))
	  (float-up i))))

;Decreases the key of the first item (in index order) for which
;the given conditional function is true.
;heap: the heap in which decrease-key will be called
;condit: the condit which must hold true for the item
;key: the new key for the item
(defun decrease-key-by-data (heap condit key)
  (decrease-key heap 
				(loop for i below (heap-len heap) 
					  when (funcall condit (heap-get-i heap i))
					  return i)
				key))

;inserts an item into the heap with the given key and data. The key and data
;will be organized into a list of the form (key data)
;heap: the heap into which the key and data are being inserted
;key: the key of the item being inserted
;data: the data of the item being inserted
(defun heap-insert (heap key data)
  (setf (heap-len heap) (1+ (heap-len heap)))
  (when (> (heap-len heap) (length (heap-arr heap)))
	(setf (heap-arr heap) (adjust-array (heap-arr heap) (* 2 (heap-len heap)))))
  (setf (aref (heap-arr heap) (1- (heap-len heap))) (list (1+ key) data))
  (decrease-key heap (1- (heap-len heap)) key))

