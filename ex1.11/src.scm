#; A function f is defined by the rule that:
#; f(n) = { n if n < 3,
#;        { f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3.
#; Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

(define (f-r n) 
    (cond ((< n 3) n)
          (else (+ (f-r (- n 1)) 
                   (* 2 (f-r (- n 2))) 
                   (* 3 (f-r (- n 3))) ))
    )
)

(define (f-i n)
    (define (f-iter count a b c )
        (display a) (newline)
        (cond ((< n 3) n)
              ((<= count 0) a)
              (else (f-iter (- count 1) (+ a (* 2 b) (* 3 c)) a b))))
    (display "1") (newline)
    (f-iter (- n 2) 2 1 0))

(f-i 20)
(newline)
