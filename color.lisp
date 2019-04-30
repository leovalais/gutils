(in-package :gutils)

(deftype rgb (&optional (range '(real 0 1)))
  `(array ,range (3)))

(deftype rgb255 ()
  '(rgb (integer 0 255)))

(defun rgb (&optional (r 0) (g 0) (b 0))
  (the rgb (make-array 3 :initial-contents (list r g b))))

(defun rgb-r (rgb)
  (declare (type rgb rgb))
  (aref rgb 0))

(defun rgb-g (rgb)
  (declare (type rgb rgb))
  (aref rgb 1))

(defun rgb-b (rgb)
  (declare (type rgb rgb))
  (aref rgb 2))

(defun rgb-* (c d)
  (declare (type rgb c d))
  (the rgb (map 'vector #'* c d)))
