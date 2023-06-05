#lang racket/base ;https://onecompiler.com/racket/

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
  (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (print a n)
    (display "expmod(")
    (display a)
    (display ", ")
    (display n)
    (display ") = ") 
    (display (expmod a n n)) (newline))

(define (fermat-test n)
  (define (try-it a)
    ;(print a n)
    (cond ((not (= (expmod a n n) a)) 1)
          ((> a 0) (try-it (- a 1)))
          (else 0)))
  (try-it (- n 1)))

(define (prime? n) 
  (= (fermat-test n) 0))
  
      
(display (prime? 561)) (newline)
(display (prime? 1105)) (newline)
(display (prime? 1729)) (newline)
(display (prime? 2465)) (newline)
