;exercise 1.7
;Square Roots by Newton’s Method

(define (sqrt-iter prev guess x) 
    (if (good-enough? prev guess)
        guess
        (sqrt-iter guess (improve guess x) x)
        )
)

(define (improve guess x)
    (average guess (/ x guess))
)

(define (average x y)
        (/ (+ x y) 2)
)

(define (good-enough? prev guess) 
    (
        < (abs (- prev guess )) 1e-9
        
    )
)

(define (abs x) (if (< x 0) (- x)  x))
(define (square x) (* x x))

(define (sqrt x) 
    (sqrt-iter 0.0 1.0 x))

(display(sqrt 3000000000000000))
(newline)
(display(sqrt 1/3000000000000000))
