
#lang racket

(require racket/base)

(provide (struct-out vec)
         vec-add
         vec-sub
         vec-mult
         vec-magnitude
         vec-normalize
         vec-limit
         vec-clamp)

(define-struct vec (x y) #:transparent)
;; make-vec: Number Number -> Vec
;; interpretation:
;;  - x is the x component of the vector
;;  - y is the y component of the vector

;; vec-add: Vec Vec ... -> Vec
;; consumes: a variable number of vecs
;; produces: a vec where
;;            - x is the sum of the x components of all the give vecs
;;            - y is the sum of the y components of all the given vecs
(define (vec-add . vs)
  (foldl (lambda (v1 v2) (make-vec (+ (vec-x v1) (vec-x v2))
                                   (+ (vec-y v1) (vec-y v2)))) 
         (make-vec 0 0) 
         vs))

;; vec-sub: Vec Vec -> Vec
;; consumes: two vectors
;; produces: a vec where
;;            - x is the result of subtracting the x component of the first given vec from the second give vec
;;            - y is the result of subtracting the y component of the first given vec from the second give vec
(define (vec-sub v1 v2)
  (make-vec (- (vec-x v1) (vec-x v2))
            (- (vec-y v1) (vec-y v2))))

;; vec-mult: Vec Number -> Vec
;; consumes: a vec and number
;; produces: a vec where
;;            - x is the result of multiplying the x component of the given vec by the given number
;;            - y is the result of multiplying the y component of the given vec by the given number
(define (vec-mult v n)
  (make-vec (* (vec-x v) n)
            (* (vec-y v) n)))

;; vec-div: Vec Number -> Vec
;; consumes: a vec and number
;; produces: a vec where
;;            - x is the result of dividing the x component of the given vec by the given number
;;            - y is the result of dividing the y component of the given vec by the given number
(define (vec-div v n)
  (make-vec (/ (vec-x v) n) 
            (/ (vec-y v) n)))

;; vec-magnitude: Vec -> Nuber
;; consumes: a vec
;; produces: the magnitude of the given vec
(define (vec-magnitude v)
  (sqrt (+ (sqr (vec-x v)) 
           (sqr (vec-y v)))))

;; vec-narmalize: Vec -> Vec
;; consumes: a vec
;; produces: the given vector normlized
(define (vec-normalize v)
  (vec-div v (vec-magnitude v)))

(define (vec-limit v n)
  (if (> (sqr (vec-magnitude v)) (sqr n))
      (vec-mult (vec-normalize v) n)
      v))

;; vec-clamp: Vec Vec Vec -> Vec
;; consumes: a vec, a minimum vec, and a maximum vec
;; produces: a vec where
;;             x is min-x if x < min-x
;;                  max-x if x > max-x
;;                  x otherwise
;;             y is min-y if y < min-y
;;                  max-y if y > max-y
;;                  y otherwise
(define (vec-clamp v min max)
  (make-vec (cond [(< (vec-x v) (vec-x min)) (vec-x min)]
                  [(> (vec-x v) (vec-x max)) (vec-x max)]
                  [else (vec-x v)])
            (cond [(< (vec-y v) (vec-y min)) (vec-y min)]
                  [(> (vec-y v) (vec-y max)) (vec-y max)]
                  [else (vec-y v)])))