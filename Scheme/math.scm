(define (power m n) ; m raised to nth power
        (if (= n 1) m
            (* m (power m (- n 1)))
        )
)

(define (log m q) ; log base m of q
        (if (= m q) 1
            (+ 1 (log m (/ q m)))
        )
)

(define (fact a) ; returns a!
        (if (= a 0) 1
            (+ a (fact (- a 1)))
        )
)

(define (comb n k) ; returns n choose k
        (/ (fact n) (* (fact k) (fact (- n k))))
)
