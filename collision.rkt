#lang racket

(require 2htdp/image
         (only-in mred make-bitmap bitmap-dc%)
         (only-in 2htdp/private/image-more render-image)
         lang/posn)

(provide collision?)

;; collision? Image Posn Image Posn -> Boolean
;; consumes: image1 is an image
;;           location1 is the location where image1 is drawn
;;           image2 is an image
;;           location2 is the location where image2 is drawn
;; produces: true if the first image collides with the second image
;;           false otherwise
(define (collision? image1 x1 y1 image2 x2 y2)
  (and (bounding-box.interset? image1 x1 y1 image2 x2 y2)
       (overlap? image1 x1 y1 image2 x2 y2)))

(define-struct rect (x1 y1 x2 y2))

(define (rect-translate r dX dY)
  (make-rect (+ (rect-x1 r) dX)
             (+ (rect-y1 r) dY)
             (+ (rect-x2 r) dX)
             (+ (rect-y2 r) dY)))

(define (rect-interset? rA rB)
  (and (< (rect-x1 rA) (rect-x2 rB))
       (> (rect-x2 rA) (rect-x1 rB))
       (< (rect-y1 rA) (rect-y2 rB))
       (> (rect-y2 rA) (rect-y1 rB))))

(define (bounding-box image x y)
  (rect-translate (make-rect 0 0 (image-width image) (image-height image))
                  (- x (ceiling (/ (image-width image) 2)))
                  (- y (ceiling (/ (image-height image) 2)))))

(define (bounding-box.interset? image1 x1 y1 image2 x2 y2)
  (rect-interset? (bounding-box image1 x1 y1)
                  (bounding-box image2 x2 y2)))

(define (overlap? image1 x1 y1 image2 x2 y2)
  (local [(define m1 (image->mask image1))
          (define w1 (image-width image1))
          (define h1 (vector-length m1))
          (define m2 (image->mask image2))
          (define w2 (image-width image2))          
          (define h2 (vector-length m2))
          (define dx (round (- (- x2 (/ w2 2)) (- x1 (/ w1 2)))))
          (define dy (round (- (- y2 (/ h2 2)) (- y1 (/ h1 2)))))]
    (for/or ((y (in-range (max 0 (- dy)) (min (- h1 dy) h2))))
      (not (zero? (bitwise-and (vector-ref m1 (+ y dy))
                               (arithmetic-shift (vector-ref m2 y) dx)))))))

(define (image->mask image)
  (define w (image-width image))
  (define h (image-height image))
  (define bm (make-bitmap w h))
  (define bdc (make-object bitmap-dc% bm))
  (render-image image bdc 0 0)
  (for/vector ((y (in-range h)))
    (define alpha-bytes (make-bytes (* 4 w)))
    (send bdc get-argb-pixels 0 y w 1 alpha-bytes #t)
    (for/sum ((x (in-range w)))
      (if (zero? (bytes-ref alpha-bytes (* 4 x)))
          0
          (arithmetic-shift 1 x)))))

(define (mask-ref mask x y)
  (cond [(< y 0) 0]
        [(<= (vector-length mask) y) 0]
        [else (bitwise-and 1 (arithmetic-shift (vector-ref mask y) (- x)))]))

