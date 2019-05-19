(defpackage :gutils
  (:use :cl)
  (:export
    ;; vector
    vect-dim
    vect
    V2
    V3
    V4
    v
    list->v
    scalar->v
    v0
    v[]
    v-dim
    v-map
    v+
    v-
    v*
    v^
    v=
    v~
    v-zerop
    v-norm
    v-unit
    vx
    vy
    vz
    vt
    v-bind

    ;; matrix
    matrix
    m
    mat
    v->m
    m->v
    m-dim
    m-rows
    m-cols
    m[]
    m-row[]
    m-col[]
    m-iter
    m-map
    m+
    m-
    m*

    ;; color
    rgb
    rgb255
    rgb-r
    rgb-g
    rgb-b
    rgb-*

    ;; image
    image
    size
    pixels
    pixel
    save
    ppm-image))
