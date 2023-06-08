#lang sicp

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
  (else (remainder (* base (expmod base (- exp 1) m)) m))))
  
(define (fast-prime2? n times)

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

  (fp-iter times n))


(display (fast-prime2? 3 10)) (newline)
(display (fast-prime2? 9 10)) (newline)
(display (fast-prime2? 561 10)) (newline)