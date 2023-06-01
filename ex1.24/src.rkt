#lang sicp

(define (square n) (* n n))

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

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
  (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
  (else #f))) 

(define (timed-prime-test1 n)
  (start-prime-test1 n (runtime)))
  
(define (timed-prime-test2 n)
  (start-prime-test2 n (runtime)))

(define (start-prime-test1 n start-time)
  (cond ((fast-prime? n 20)
  (report-prime (- (runtime) start-time) n))))
 
(define (start-prime-test2 n start-time)
  (cond ((prime? n)
  (report-prime (- (runtime) start-time) n))))

(define (report-prime elapsed-time n)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from to)
  (timed-prime-test1 from)
  ;(timed-prime-test2 from)
  (cond ((< from to) 
      (search-for-primes (+ from 2) to))))

(define (even? n)
  (= (remainder n 2) 0))
      
(define (test-aver from to)
  (define (iter from sum count)
    (define (timed-prime-test n)
      (start-prime-test n (runtime)))
  
    (define (start-prime-test n start-time)
      (if (fast-prime? n 20)
           (iter (+ from 2) (+ sum (- (runtime) start-time)) (+ count 1))
           (iter (+ from 2) sum count)))

    (cond ((< from to)
        (timed-prime-test (+ from 2)))
     (else (display (/ sum count)) (newline))))
  
  (if (even? from)(iter (+ from 1) 0.0 0)
    (iter from 0.0 0)))

(test-aver 1001 9999)
(test-aver 10001 99999)
(test-aver 100001 999999)
(test-aver 1000001 9999999)