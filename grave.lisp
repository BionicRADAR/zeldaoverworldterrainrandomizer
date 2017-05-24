;grave.lisp
;Author: Nathaniel Schleicher
;
;This file is supposed to handle making and altering the grave biome for
;the Zelda Overworld Randomizer

(defconstant +normal-grave-tile+ 20)
(defconstant +pushable-grave-tile+ 41)
;Note: the pushable grave column is independent of its neighbors;
;	   thus, if needed, it can be changed to fit a different column style
(defconstant +pushable-grave-column+ 136)

(defconstant +NW-screen+ (copy-seq (aref *screens* 31)))
(defconstant +N-screen+ '(2 2 7 130 7 7 130 7 7 130 7 7 136 7 2 2))
(defconstant +NE-screen+ (aref *screens* 32))
(defconstant +W-screen+ (copy-seq (aref *screens* 47)))
(defconstant +E-screen+ (substitute 21 22 (aref *screens* 48)))
(defconstant +SW-screen+ (substitute 2 181 (aref *screens* 62)))
(defconstant +S-screen+ '(2 2 8 147 8 8 147 8 8 147 8 8 147 8 2 2))
(defconstant +SE-screen+ (copy-seq (aref *screens* 63)))

(defun place-grave-screens (screens biomes grave-ids))

(defun make-grave (screens biomes horizontals verticals grave-ids))
