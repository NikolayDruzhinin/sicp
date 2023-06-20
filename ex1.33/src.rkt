#lang sicp
;a)  the sum of the squares of the prime numbers in the interval a to b
;    (assuming that you have a prime? predicate already written)

;b)  the product of all the positive integers less than n that
;    are relatively prime ton (i.e., all positive integersi < n
;    such that GCD(i, n) = 1)


;(a)
(define (filtered-accumulate predicate? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate? a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (sum a b) (+ a b))
(define (square n) (* n n))
(define (next n)
  (if (even? n) (+ n 1) (+ n 2)))

(define (prime? n)
  (define (fp-iter times n)
    (cond ((= times 0) #t)
          ((mr-test n) (fp-iter (- times 1) n))
          (else #f)))

  (define (mr-test n)
    (define (try-it a)
      (= (expmod-mr a n n) a))
    (try-it (+ 1 (random (- n 1)))))

  (define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square-mr (expmod-mr base (/ exp 2) m)) m))
  (else (remainder (* base (expmod-mr base (- exp 1) m)) m))))

  (define (square-mr x)
    (define y (remainder (square x) n))
    (if (and (= y 1) (not (eq? x 1)) (not (eq? x (- n 1))))
        0
        y))

  (fp-iter 10 n))

(filtered-accumulate prime? sum 0 square 2 next 2)  ;2^2
(filtered-accumulate prime? sum 0 square 2 next 5)  ;2^2 + 3^2 + 5^2
(filtered-accumulate prime? sum 0 square 2 next 10) ;2^2 + 3^2 + 5^2 + 7^2
(filtered-accumulate prime? sum 0 square 2 next 11) ;2^2 + 3^2 + 5^2 + 7^2 + 11^2

;(b)

(define (filtered-accumulate-gcd predicate? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate? b a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (product n m) (* n m))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (relative-prime? a b)
  (= (gcd a b) 1))

(define (identity n) n)

(define (next-rp n) (+ n 1))

(filtered-accumulate-gcd relative-prime? product 1 identity 1 next-rp 7) ;2*3*4*5*6
(filtered-accumulate-gcd relative-prime? product 1 identity 1 next-rp 24) ;5*7*11*13*17*19*23