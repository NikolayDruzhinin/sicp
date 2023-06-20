; (a) The sum procedure is only the simplest of a vast number of similar
;     abstractions that can be captured as higher-order procedures. Write
;     an analogous procedure called product that returns the product of
;     the values of a function at points over a given range. Show how to
;     define factorial in terms of product. Also use product to compute
;     approximations to π using the formula
;     pi/4 = (2 * 4 * 4 * 6 * 6 * 8...) / (3 * 3 * 5 * 5 * 7 * 7...)

; (b) If your product procedure generates a recursive process, write one
;     that generates an iterative process. If it generates an iterative process,
;     write one that generates a recursive process.

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