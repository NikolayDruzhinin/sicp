#lang sicp
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                 (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (prod-comb x y) (* x y))
(define (sum-comb x y) (+ x y))

(define (pi-rec n)
  (define (next x) (+ x 1))
  (define (pi-term x)
    (if (even? x) (/ (+ x 2) (+ x 1)) (/ (+ x 1) (+ x 2))))
  (* (accumulate-rec prod-comb 1.0 pi-term 1 next n) 4))

(define (pi-iter n)
  (define (next x) (+ x 1))
  (define (pi-term x)
    (if (even? x) (/ (+ x 2) (+ x 1)) (/ (+ x 1) (+ x 2))))
  (* (accumulate-iter prod-comb 1.0 pi-term 1 next n) 4))

(pi-rec 100)
(pi-iter 100)