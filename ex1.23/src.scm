;define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input
;plus 2. Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1).

#lang racket/base

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n) 
  (newline)
  (display n)
  (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) 
  (find-divisor n 2))
  
(define (next n)
  (if (= n 2) 3
    (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next n)))))

(define (divides? a b) 
  (= (remainder b a) 0))

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (search-for-primes from to)
  (timed-prime-test from)
  (cond ((< from to) 
      (search-for-primes (+ from 2) to))))

(search-for-primes 1000000000001 1000000000500)
