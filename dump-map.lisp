(defun dump-map (file-name last-item test-name biomes)
  (let ((upper-limit
		  (cdr (assoc last-item
					  '((biomes . 3) (edges . 8) (columns . 15) 
									 (screens 19))))))
  (with-open-file (outfile file-name :direction :output :if-exists :append
						   :if-does-not-exist :create)
	(mapc 
	  (lambda (name item)
		(format outfile "(defparameter ~a ~a)" name item))
	  (subseq 
		(mapcar (lambda (name) (concatenate 'string "*" test-name "-" name "*"))
				'("biomes" "biome-coords-list" "special-biome-identifiers"
				  "horizontal-edges" "vertical-edges" "lake-wall" 
				  "secret-passage" "connect-graph-array"
				  "new-byte-columns" "new-columns" "unused-cols"
				  "column-names" "simplifications" "swapped-columns"
				  "replaced-columns" "name-screens" "new-screens" 
				  "new-base-screens" "edges-adapted"))
		0 upper-limit)
	  (subseq
		`(,biomes ,*biome-coords-list* ,*special-biome-identifiers*
		  ,*horizontal-edges* ,*vertical-edges* ,*lake-wall* ,*secret-passage*
		  ,*connect-graph-array* ,*new-byte-columns* ,*new-columns*
		  ,*unused-cols* ,*column-names* ,*simplifications* ,*swapped-columns*
		  ,*replaced-columns* ,*name-screens* ,*new-screens* 
		  ,*new-base-screens* ,*edges-adapted*)
		0 upper-limit)))))
