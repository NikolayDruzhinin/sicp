; Design a procedure that evolves an iterative exponentiation
; process that uses successive squaring and uses a logarithmic
; number of steps, as does fast-expt.
; (Hint: Using the observation that (bn=2)2 = (b2)n=2, keep,
; along with the exponent n and the base b, an additional
; state variable a, and define the state transformation in such
; a way that the product abn is unchanged from state to state.
; At the beginning of the process a is taken to be 1, and the
; answer is given by the value of a at the end of the process.
; In general, the technique of defining ana invariant quantity
; that remains unchanged from state to state is a powerful
; way to think about the design of iterative algorithms.)

#lang sicp
(define (even? n)
  (= (remainder n 2) 0))
(define (exp x n)
  (define (exp-iter x n a)
    (cond ((= n 0) a)
          ((even? n) (exp-iter (* x x) (quotient n 2) a))  
          (else (exp-iter (* x x) (quotient n 2) (* x a)))))
  (exp-iter x n 1))
(exp 2 12)
(exp 4 6)
(exp 16 2)