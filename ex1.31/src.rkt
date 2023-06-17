#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter n res)
    (if (> n b)
        res
        (iter (next n) (* res (term n)))))
  (iter a 1.0))

(define (factorial x)
  (define (ident x) x)
  (define (next x) (+ 1 x))
  (product ident 2 next x))

(define (square x) (* x x))

(define (pi-rec n)
  (define (pi-term x)
    (/ (* x (+ x 2)) (square (+ x 1.0))))
  (define (pi-next x)
    (+ x 2))
  (* (product pi-term 2 pi-next n) 4))

(define (pi-iter n)
  (define (pi-term x)
    (/ (* x (+ x 2)) (square (+ x 1))))
  (define (pi-next x)
    (+ x 2))
  (* (product-iter pi-term 2 pi-next n) 4))

(factorial 5)
(factorial 10)
(pi-rec 100)
(pi-iter 100)
(pi-rec 1000)
(pi-iter 1000)