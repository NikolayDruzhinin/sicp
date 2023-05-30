#lang sicp
(define (even? n)
  (= (remainder n 2) 0))
(define (double n) (+ n n))

(define (halve n)
  (quotient n 2))

(define (print x y n)
    (display "(")
    (display x)
    (display ", ")
    (display y)
    (display ", ")
    (display n)
    (display ")") (newline))

(define (* a b)
  (define (mul x y n)
    (cond ((= n 0) y)
          ((even? n) (mul (double x) y (halve n)))
          (else (mul (double x) (+ y x) (halve n)))))
  (mul a 0 b))

(display (* 4 40)) (newline)
(display (* 5 76)) (newline)(display (* 8 3)) (newline)
(display (* 9 9)) (newline)(display (* 5 5)) (newline)
(display (* 0 0)) (newline)