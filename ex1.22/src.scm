; write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range.

#lang racket/base ;https://onecompiler.com/racket/
(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((prime? n) 
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

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) 
  (= (remainder b a) 0))

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (search-for-primes from to)
  (timed-prime-test from)
  (cond ((< from to) 
      (search-for-primes (+ from 2) to))))

(search-for-primes 1000000001 1000000500)
(search-for-primes 10000000001 10000000500)
(search-for-primes 100000000001 100000000500)
(search-for-primes 1000000000001 1000000000500)
