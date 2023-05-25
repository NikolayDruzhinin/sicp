#; Write a procedure that computes elements of Pascal’s triangle by means of a recursive process

(define (f n)
    (define (iter product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1))))
    (iter 1 1))
(define (pascal-tri n m)
    (cond ((= m n) (display "1") 
              (newline) 
              (pascal-tri (- n 1) 0))
          ((< m n) (display (/ (f n) (* (f m) (f (- n m))))) 
              (display " ")
              (pascal-tri n (+ m 1))))
)
(pascal-tri 100 0)
