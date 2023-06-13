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
    (+ x h))
  (define (simp-sum term aa next bb coeff)
    (define factor
      (cond ((or (= coeff 0) (= coeff n)) 1)
            ((= (remainder coeff 2) 1) 4)
            (else 2)))
    (if (> aa bb)
        0
        (+ (* factor (term aa))
           (simp-sum term (next aa) next bb (+ coeff 1)))))
  (* (simp-sum f a next-h (- b a) 0) (/ h 3.0)))

(integral cube 0 1 0.001)
(simpson-integral cube 0 1 1000)

(integral cube 0 5 0.001)
(simpson-integral cube 0 5 1000)