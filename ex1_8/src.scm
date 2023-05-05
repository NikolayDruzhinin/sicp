;exercise 1.8
;Cube Roots by Newton’s Method

(define (cbrt-iter prev guess x) 
    (if (good-enough? prev guess)
        guess
        (cbrt-iter guess (approx guess x) x)
        )
)

(define (approx y x)
        (/ (+ (/ x (square y)) (* 2 y)) 3)
)

(define (good-enough? prev guess) 
    (
        < (abs (- prev guess )) 1e-9
        
    )
)

(define (abs x) (if (< x 0) (- x)  x))
(define (square x) (* x x))

(define (sqrt x) 
    (cbrt-iter 0.0 1.0 x))

(display(sqrt 3000000000000000))
(newline)
(display(sqrt 1/3000000000000000))
