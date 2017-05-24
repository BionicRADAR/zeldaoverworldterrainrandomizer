(defparameter *containing-directory* "zeldadisplay")

(defun display-map (fname screen-list screens get-column palettes specials
								mapwidth mapheight screenwidth screenheight)
  (with-open-file (*standard-output* fname :direction :output 
										   :if-exists :supersede)
	(format t "~d ~d ~d ~d~&" mapwidth mapheight screenwidth screenheight)
	(mapcar (lambda (scr palette spec) (display-screen scr screens get-column
															palette spec))
			screen-list palettes specials)))

(defun display-screen (screen screens get-column palette spec)
  (format t "~a ~a " (car palette) (cadr palette))
  (if (listp spec)
	(if spec
	  (format t "~d ~d~&" (car spec) (cadr spec))
	  (format t "null~&"))
	(format t "~a~&" spec))
  (mapc (lambda (x) (format t "~{~2,'0x ~}~&" x))
		(apply #'mapcar #'list
				(mapcar (lambda (x) (funcall get-column x)) 
						(coerce (aref screens screen) 'list)))))

(defun call-map-painter (fname)
  (ext:shell (concatenate 'string "java ZeldaDisplay " fname)))

(defun make-png (fname screen-list screens get-column palettes 
						specials mapwidth mapheight screenwidth screenheight) 
  (let ((old-directory (ext:cd)))
	(ext:cd "zeldadisplay")
	(display-map fname screen-list screens get-column palettes specials
				 mapwidth mapheight screenwidth screenheight)
	(ext:shell 
	  (concatenate 'string "java ZeldaDisplay " fname " " fname ".png"))
	(ext:cd old-directory)))

(defun format-columns-for-visual (columns-array)
  (mapcar (lambda (x)
			(format t "~2,'0x" (rev-convert-col-num x))
			(format t "~{ ~x~}~&" (aref columns-array x)))
		  (interval 0 (1- (length columns-array)))))

(defun output-columns-for-display (columns-array fname)
  (with-open-file (*standard-output* fname :direction :output 
									 :if-exists :supersede)
	(format-columns-for-visual columns-array)))
