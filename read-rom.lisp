;; A program intended to help exploring a rom
;; (specifically, the Legend of Zelda rom).
;; While some of it might work for any rom, in the end, I will try to use it to
;; find the map data in the LoZ rom, which won't be useful for other roms
;; Author: Nathaniel Schleicher

(defparameter *byte-array* 'nil)

(defun read-rom (rom-fname)
  (with-open-file (rom rom-fname :direction :input 
					   :element-type '(unsigned-byte 8))
	(loop do 
		  (loop repeat 16 do (format t "~2,'0x " (read-byte rom)))
		  do (fresh-line))))

(defun write-rom (outfile rom-fname)
  (with-open-file (*standard-output* outfile :direction :output
									 :if-exists :supersede)
	(read-rom rom-fname)))

(defun read-rom-to-list (rom-fname)
  (with-open-file (rom rom-fname :direction :input 
					   :element-type '(unsigned-byte 8))
	(labels ((read-next-byte (acc)
				(let ((next-byte (read-byte rom nil)))
				  (if next-byte
					(read-next-byte (cons next-byte acc))
					(reverse acc)))))
	  (read-next-byte nil))))


(defun read-rom-to-array (rom-fname)
  (setf *byte-array* (coerce (read-rom-to-list rom-fname) 'array)))

(defun n-row (value arr pos repeats)
  (if (zerop repeats)
	t
	(if (and (< pos (length arr)) (eql (aref arr pos) value))
	  (n-row value arr (1+ pos) (1- repeats))
	  nil)))

(defun find-first-column (arr pos locs)
  (if (>= (+ 10 pos) (length arr))
	locs
	(if (n-row (aref arr pos) arr pos 5)
	  (if (eql (aref arr pos) (aref arr (+ 5 pos)))
		(find-first-column arr (1+ pos) locs)
		(if (n-row (aref arr (+ pos 5)) arr (+ pos 5) 6)
		  (find-first-column arr (+ pos 6) (cons pos locs))
		  (find-first-column arr (+ pos 5) locs)))
	  (find-first-column arr (1+ pos) locs))))

(defun find-fourth-column (arr pos locs)
  (if (>= (+ 10 pos) (length arr))
	locs
	(if (n-row (aref arr pos) arr pos 3)
	  (let ((prevs (list (aref arr pos))))
		(if (member (aref arr (+ 3 pos)) prevs)
		  (find-fourth-column arr (1+ pos) locs)
		  (progn (push (aref arr (+ 3 pos)) prevs)
			(if (member (aref arr (+ 4 pos)) prevs)
			  (find-fourth-column arr (+ pos 4) locs)
			  (progn (push (aref arr (+ 4 pos)) prevs)
				(if (member (aref arr (+ 5 pos)) prevs)
				  (find-fourth-column arr (+ pos 4) locs)
				  (if (n-row (aref arr (+ pos 5)) arr (+ pos 5) 3)
					(progn (push (aref arr (+ pos 5)) prevs)
					  (if (member (aref arr (+ pos 8)) prevs)
						(find-fourth-column arr (+ pos 6) locs)
						(if (n-row (aref arr (+ pos 8)) arr (+ pos 8) 3)
						  (find-fourth-column arr (+ pos 8) (cons pos locs))
						  (find-fourth-column arr (+ pos 5) locs))))
					(find-fourth-column arr (+ pos 8) locs))))))))
	  (find-fourth-column arr (1+ pos) locs))))

(defmacro make-sequence-finder (seq)
  `(defun findthissequence (arr pos locs)
	(if (> (+ ,(apply #'+ (mapcar #'cadr seq)) pos) (length arr))
	  locs
	  (if (n-row (aref arr pos) arr pos ,(cadar seq))
		(let ((prevs (list (aref arr pos))))
		  ,(labels ((inner-f (preseq postseq)
			(let* ((member-length 
					 (length (member (caar postseq) (mapcar #'car preseq))))
				   (current-offset `(+ pos ,(apply #'+ (mapcar #'cadr preseq))))
				   (member-condition
					 (if (zerop member-length)
					   `(member (aref arr ,current-offset)
										 prevs)
					   `(not (eql (nth ,(- (length preseq) member-length) prevs)
								(aref arr ,current-offset)))))
				   (n-row-cond `(n-row (aref arr ,current-offset) arr 
									   ,current-offset ,(cadar postseq))))
			  `(if ,member-condition
				(findthissequence arr (1+ pos) locs)
				(if ,n-row-cond
				  ,(if (eql (length postseq) 1)
					`(findthissequence arr (1+ pos) (cons pos locs))
					`(progn (push (aref arr ,current-offset) prevs)
					,(inner-f (cons (car postseq) preseq) (cdr postseq))))
				  (findthissequence arr (1+ pos) locs))))))
			 (inner-f (list (car seq)) (cdr seq))))
		(findthissequence arr (1+ pos) locs)))))

(defun find-map (zeldafname modfname)
  (with-open-file (zelda zeldafname :direction :input 
						 :element-type '(unsigned-byte 8))
	(with-open-file (modmap modfname :direction :input
						 :element-type '(unsigned-byte 8))
	  (princ (loop for i from 0
			when (not (eql (read-byte zelda) (read-byte modmap)))
			do (format t "~d ~%" i))))))

(defun find-columns (fname)
  (let ((columns-list (nbutlast (nthcdr 89064(read-rom-to-list fname)) 41060)))
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


(defun bytes-near (fname bytenum)
  (with-open-file (mod-rom fname :direction :input 
						   :element-type '(unsigned-byte 8))
	(loop repeat bytenum do (read-byte mod-rom))
	(loop repeat 5 do (format t "~x ~&" (read-byte mod-rom)))))

(defun find-diff (fname0 fname1)
  (with-open-file (f0 fname0 :direction :input)
	(with-open-file (f1 fname1 :direction :input)
	  (loop for i from 0
			do (let* ((line0 (read-line f0))
					  (line1 (read-line f1))
					  (loc0 (position #\$ line0))
					  (loc1 (position #\$ line1))
					  (pc0 (substring line0 loc0 (+ loc0 5)))
					  (pc1 (substring line1 loc1 (+ loc1 5))))
				 (unless (equal pc0 pc1)
				   (princ i)
				   (fresh-line)
				   (princ line0)
				   (fresh-line)
				   (princ line1)
				   (fresh-line)
				   return))))))

(compile 'read-rom-to-list)
(compile 'find-first-column)
(compile 'find-fourth-column)
(compile 'find-columns)

(defparameter *rom-columns* (find-columns "zelda-rom.nes"))

(read-rom-to-array "zelda-rom.nes")

(defparameter *byte-columns*
  (coerce (mapcar (lambda (x) (aref *byte-array* x)) 
		  (interval 89064 (- (length *byte-array*) 41062))) 'array))

(defun shared-tiles (col-num)
  (let ((pred-done nil) (col-done nil) (succ-done nil) 
						(col-count (convert-column-number col-num)))
  (loop for x below (length *byte-columns*)
		when (> (aref *byte-columns* x) #x80)
		do (decf col-count)
		when (eql col-count 0) do (progn 
								  (when (not pred-done)
									(setf pred-done t)
									(format t "~&Predecessor start at loc ~x~&"
											x))
								  (format t "~2,'0x " 
										  (aref *byte-columns* x)))
		when (eql col-count -1) do (progn 
								  (when (not col-done)
									(setf col-done t)
									(format t "~&Column start at loc ~x~&"
											x))
								  (format t "~2,'0x " 
										  (aref *byte-columns* x)))
		when (eql col-count -2) do (progn 
								  (when (not succ-done)
									(setf succ-done t)
									(format t "~&Successor start at loc ~x~&"
											x))
								  (format t "~2,'0x " 
										  (aref *byte-columns* x))))))
