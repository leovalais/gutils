(in-package :gutils)

(deftype matrix (&optional (rows '*) (cols '*) (type 'number))
  `(array ,type (,rows ,cols)))

(defun m (&rest lists)
  (let ((vec (make-array (list (length lists)
                               (length (car lists)))
                         :initial-contents lists)))
    (the matrix vec)))

(defmacro mat (&rest lists)
  `(m ,@(mapcar (lambda (row)
                  `(list ,@row))
                lists)))

(defun v->m (v &key (type :row))
  (declare (type vect v)
           (type (member :row :column) type))
  (if (eql type :column)
      (apply #'m (map 'list #'list v))
      (m (map 'list #'identity v))))

(defun m->v (m)
  (declare (type (or (matrix 1 *)
                     (matrix * 1))
                 m))
  (etypecase m
    ((matrix * 1)
     (apply #'v (loop :for i :below (m-rows m)
                      :collecting (aref m i 0))))
    ((matrix 1 *)
     (apply #'v (loop :for i :below (m-cols m)
                      :collecting (aref m 0 i))))))

(defun m-dim (m)
  (declare (type matrix m))
  (the list (array-dimensions m)))

(defun m-rows (m)
  (first (m-dim m)))
(defun m-cols (m)
  (second (m-dim m)))


(defun m[] (row col m)
  (declare (type unsigned-byte row col)
           (type matrix m))
  (the number (aref m row col)))

(defun (setf m[]) (n row col m)
  (declare (type number n)
           (type matrix m)
           (type unsigned-byte row col))
  (setf (aref m row col)
        n))


(defun m-row[] (row m)
  (declare (type unsigned-byte row)
           (type matrix m))
  (let ((v (aops:generate (lambda (i)
                            (aref m row (car i)))
                          (m-cols m)
                          :subscripts)))
    (the vect v)))
;; TODO bench more efficient
;; (let* ((nc (m-cols m))
;;        (v (v0 nc)))
;;   (dotimes (i nc)
;;     (setf (aref v i)
;;           (aref m row i)))
;;   (the vect v)))

(defun (setf m-row[]) (v row m)
  (declare (type vect v)
           (type unsigned-byte row)
           (type matrix m))
  (assert (= (v-dim v)
             (m-cols m))
          nil "wrong dimensions: given row ~S cannot fit inside a ~S matrix ~S" v (m-dim m) m)
  (aops:each-index i
    (setf (aref m row i)
          (aref v i))))

(defun m-col[] (col m)
  (declare (type unsigned-byte col)
           (type matrix m))
  (let* ((nr (m-rows m))
         (v (v0 nr)))
    (dotimes (i nr)
      (setf (aref v i)
            (aref m i col)))
    (the vect v)))

(defun (setf m-col[]) (v col m)
  (declare (type vect v)
           (type unsigned-byte col)
           (type matrix m))
  (assert (= (v-dim v)
             (m-rows m))
          nil "wrong dimensions: given column ~S cannot fit inside a ~S matrix ~S" v (m-dim m) m)
  (aops:each-index i
    (setf (aref m i col)
          (aref v i))))


(defun m-iter (proc &rest ms)
  (assert ms)
  (assert (apply #'= (mapcar #'m-dim ms)))
  (destructuring-bind (rows cols)
      (m-dim (first ms))
    (loop
      :for i :below rows
      :do (loop
            :for j :below cols
            :do (apply proc (mapcar (lambda (m)
                                      (aref m i j))
                                    ms))))))

(defun m-map (fun &rest ms)
  (assert ms)
  (destructuring-bind (rows cols)
      (m-dim (first ms))
    (let ((vec (make-array (list rows cols))))
      (loop
        :for i :below rows
        :do (loop
              :for j :below cols
              :do (setf (aref vec i j)
                        (apply fun (mapcar (lambda (m)
                                             (aref m i j))
                                           ms)))))
      vec)))

(defun m+ (&rest ms)
  (let ((m (apply #'m-map #'+ ms)))
    (the matrix m)))

(defun m- (&rest ms)
  (let ((m (apply #'m-map #'- ms)))
    (the matrix m)))

(defun m* (mat1 mat2)
  (declare (type matrix mat1 mat2))
  (destructuring-bind ((m n) (n2 p))
      (list (m-dim mat1) (m-dim mat2))
    (assert (= n n2))
    (let ((res (make-array (list m p))))
      (loop
        :for i :below m
        :do (loop
              :for j :below p
              :do (setf (aref res i j)
                        (loop
                          :for k :below n
                          :summing (* (aref mat1 i k)
                                      (aref mat2 k j))))))
      res)))
