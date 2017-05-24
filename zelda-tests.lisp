(defconstant +normal-screens+ 
			 '(0 1 2 3 4 5 6 7 8 11 12 15 16 17 18 19 20 21 26 27 34 35 43 49
			   50 59 69 70 71 72 73 74 76 77 78 79
			   83 84 87 88 89 90 91 92 93 94 95 97 98 99 102 103 104 105 107 108
			   109 110 113 114 115 116))

(defun test-adapts (screen-lst)
  (flet ((get-screen (base-scr) 
					 (coerce (copy-seq (aref *screens* base-scr)) 'array)))
	(mapcar
	  (lambda (x)
		(mapcar
		  (lambda (y)
			(unless (eql x y)
			  (format t "x: ~d y: ~d~&" x y)
			  (adapt-side x y (get-screen x) (get-screen y) t *extends*)
			  (adapt-vert x y (get-screen x) (get-screen y) *extends*)))
		  screen-lst))
	  screen-lst)))

