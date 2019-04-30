(in-package :gutils)

(defclass image ()
  ((w :initform 0
      :type (integer 0 *)
      :initarg :w
      :reader w)
   (h :initform 0
      :type (integer 0 *)
      :initarg :h
      :reader h)))

(defgeneric size (image)
  (:method ((i image))
    (v (w i) (h i))))

(defgeneric pixels (image))
(defgeneric pixel (image x y))
(defgeneric (setf pixel) (color image x y))
(defgeneric save (image path))

(defclass ppm-image (image)
  ((pixels :type (array rgb255 (* *))
           :initarg :pixels)))

(defmethod pixels ((i ppm-image))
  (slot-value i 'pixels))

(defmethod pixel ((i ppm-image) x y)
  (aref (slot-value i 'pixels) y x))

(defmethod (setf pixel) (color (i ppm-image) x y)
  (setf (aref (slot-value i 'pixels) y x)
        color))

(defmethod save ((i ppm-image) path)
  (netpbm:write-to-file path (pixels i)))

(defmethod initialize-instance :after ((i ppm-image) &key)
  (let ((actual-size (list->v (array-dimensions (pixels i))))
        (expected-size (size i)))
    (unless (and (= (v-dim actual-size)
                   (v-dim expected-size))
                 (v= actual-size expected-size))
      (setf (slot-value i 'pixels)
            (make-array (list (h i) (w i))
                        :initial-element 0)))))
