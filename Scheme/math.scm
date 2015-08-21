(define (power m n)
        (if (= n 1) m
            (* m (power m (- n 1)))
        )
)

(define (mod a b) (* (- a (/ a b) b)))
(define (log m q) (/ (- q (mod q m)) m))
; small problem with log
