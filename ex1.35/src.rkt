#lang sicp

(define tolerance 0.00001)
(define (fixed-point f x)
  (define (good-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try x)
    (let ((next (f x)))
      (if (good-enough? next x)
          next
          (try next))))
  (try x))
(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(fixed-point cos 1.0)
(sqrt 25)
(sqrt 1000)

; Show that the golden ratio φ (Section 1.2.2) is a fixed point of the
; transformation x -> 1 + 1/x, and use this fact to compute φ by means
; of the fixed-point procedure

; golden ratio φ^2 = φ + 1 (/φ) ==> φ = 1 + 1/φ
; golden ratio (1 + sqroot(5)) / 2 ~ 1.6180
(fixed-point (lambda (φ) (+ 1 (/ 1 φ))) 1.0); 1.6180
