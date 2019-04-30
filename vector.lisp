(in-package :gutils)

(deftype vect-dim () '(integer (0) *))

(deftype vect (&optional (dim '*) (type 'number))
  `(array ,type (,dim)))

(deftype V2 (&optional (type 'number))
  `(vect 2 ,type))

(deftype V3 (&optional (type 'number))
  `(vect 3 ,type))

(deftype V4 (&optional (type 'number))
  `(vect 4 ,type))

(defun v (&rest xs)
  (the vect (map 'vector #'identity xs)))

(defun list->v (l)
  (declare (type list l))
  (let ((v (apply #'v l)))
    (the vect v)))

(defun v0 (d)
  (declare (type vect-dim d))
  (the vect
       (make-array d :initial-element 0)))

(defun v[] (n v)
  (declare (type vect v)
           (type vect-dim n))
  (the number (aref v n)))

(defun v-dim (v)
  (declare (type vect v))
  (the vect-dim (length v)))

(defun assert-same-dim (u v)
  (declare (type vect u v))
  (or (= (length u) (length v))
      (error "dimension assertion failed for ~S" (cons u v))))

(defun v-map (f v &rest vs)
  (declare (type vect v))
  (let ((dims (mapcar #'v-dim (cons v vs))))
    (unless (apply #'= dims)
      (error "v-map requires all vectors to have the same dimension. Found dimensions: ~S" dims)))
  (the vect (apply #'map 'vector f v vs)))

(defun v+ (&rest vs)
  (let ((v (apply #'v-map #'+ vs)))
    (the vect v)))

(defun v- (&rest vs)
  (let ((v (apply #'v-map #'- vs)))
    (the vect v)))

(defun v* (&rest vs)
  (let ((v (apply #'v-map #'* vs)))
    (the vect v)))

(defun v^ (u v)
  "Dot product of two vectors: [x1, ..., xn] ^ [y1, ..., yn] = x1y1 + ... + xnyn"
  (declare (type vect u v))
  (let* ((values (map 'vector #'* u v))
         (dot (reduce #'+ values)))
    (the number dot)))

(defun v= (u v)
  (declare (type vect u v))
  (assert-same-dim u v)
  (the boolean (every #'= v u)))

(defun v-norm (v)
  (declare (type vect v))
  (sqrt (reduce #'* v)))

(defun vx (v)
  (declare (type vect v))
  (the number (v[] v 0)))

(defun vy (v)
  (declare (type vect v))
  (the number (v[] v 1)))

(defun vz (v)
  (declare (type vect v))
  (the number (v[] v 2)))

(defun vt (v)
  (declare (type vect v))
  (the number (v[] v 3)))
