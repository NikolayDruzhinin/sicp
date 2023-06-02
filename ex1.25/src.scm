#lang racket/base ;https://onecompiler.com/racket/

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
  (else (remainder (* base (expmod base (- exp 1) m)) m))))
  
(define (expmod2 base exp m)
	(remainder (fast-expt base exp) m))

(define (fast-expt b n)
	(cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (fermat-test1 n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime1? n times)
  (cond ((= times 0) #t)
    ((fermat-test1 n) (fast-prime1? n (- times 1)))
  (else #f)))
  
(define (fermat-test2 n)
  (define (try-it a)
    (= (expmod2 a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime2? n times)
  (cond ((= times 0) #t)
    ((fermat-test2 n) (fast-prime2? n (- times 1)))
  (else #f)))

(define (even? n)
  (= (remainder n 2) 0))
      
(define (test-aver1 from to)

  (define (iter from sum count)
  
    (define (timed-prime-test n)
      (start-prime-test n (current-inexact-milliseconds)))
  
    (define (start-prime-test n start-time)
      (if (fast-prime1? n 20)
           (iter (+ from 2) (+ sum (- (current-inexact-milliseconds) start-time)) (+ count 1))
           (iter (+ from 2) sum count)))

    (cond ((< from to)
        (timed-prime-test (+ from 2)))
     (else (display (* 1000 (/ sum count))) (newline))))
  
  (if (even? from) (iter (+ from 1) 0.0 0)
    (iter from 0.0 0)))
    
(define (test-aver2 from to)

  (define (iter from sum count)
  
    (define (timed-prime-test n)
      (start-prime-test n (current-inexact-milliseconds)))
  
    (define (start-prime-test n start-time)
      (if (fast-prime2? n 20)
           (iter (+ from 2) (+ sum (- (current-inexact-milliseconds) start-time)) (+ count 1))
           (iter (+ from 2) sum count)))

    (cond ((< from to)
        (timed-prime-test (+ from 2)))
     (else (display (* 1000 (/ sum count))) (newline))))
  
  (if (even? from) (iter (+ from 1) 0.0 0)
    (iter from 0.0 0)))

(test-aver1 1001 1500) ;19.38
(test-aver2 1001 1500) ;1909.68
