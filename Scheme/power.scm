(define (power m n)
        (if (= n 1) 1
            (* m (power m (- n 1)))
        )
)
