#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

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