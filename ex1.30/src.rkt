; The sum procedure above generates a linear
; recursion. The procedure can be rewritten so that the sum
; is performed iteratively. Show how to do this by filling in
; the missing expressions in the following definition

#lang sicp

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(define (cube x)
  (* x x x))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  
  (define (next-h x)
    (+ x (* 2 h)))
  
  (define (simp-term x)
    (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h)))))
  
  (* (sum simp-term a next-h (- b (* 2 h))) (/ h 3.0)))

(integral cube 0 1 0.001)
(simpson-integral cube 0 1 1000)

(integral cube 0 5 0.001)
(simpson-integral cube 0 5 1000)