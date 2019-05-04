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
    v0
    v[]
    v-dim
    v-map
    v+
    v-
    v*
    v^
    v=
    v-norm
    vx
    vy
    vz
    vt
    v-bind

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
