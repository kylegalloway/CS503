;-------------- Lists ----------------;
; New List
(define (list . L) L)

; Length
(define (length L)
        (if (null? L) 0
            (+ 1 (length (cdr L)))
        )
)

; Append
(define (append L1 L2)
        (if (null? L1) L2
            (cons (car L1)
                  (append (cdr L1) L2)
            )
        )
)

; Add to the end
(define (addlast value L)
        (if (null? L) (list value)
            (cons (car L)
                  (addlast value (cdr L))
            )
        )
)

;Reverse
(define (reverse L) (foldl cons '() L))

; Sum all numbers (even nested)
(define (sum_nested L)
        (cond ((number? L) L)
              ((pair? L)
               (+ (sum_nested (car L))
                  (sum_nested (cdr L))
               )
              )
        )
)

; Map
(define (map f L)
        (if (null? L) L
            (cons (f (car L))
                  (map f (cdr L))
            )
        )
)

; Fold Right
(define (foldr f id L)
        (if (null? L) id
            (f (car L) (foldr f id (cdr L)))
        )
)

; Fold Left
(define (foldl f id L)
        (if (null? L) id
            (foldl f (f (car L) id) (cdr L))
        )
)

; Filter
(define (filter f L)
        (if (null? L) L
            (if (f (car L))
                (cons (car L) (filter f (cdr L)))
                (filter f (cdr L))
            )
        )
)

;-------------- Trees ----------------;
; Binary Search Trees

; Binary Search Tree Left
(define (left T) (car (cdr T)))

; Binary Search Tree Right
(define (right T) (car (cdr (cdr T))))

; Binary Search Tree Search (returns boolean)
(define (search value T) ; T is a BST
        (if(null? T) #f
           (if (= value (car T)) #t
               (if (< value (car T))
                   (search value (left T))
               )
           )
        )
        (search value (right T))
)

;-------------- Math -----------------;
; Power
(define (power m n) ; m raised to nth power
        (if (= n 1) m
            (* m (power m (- n 1)))
        )
)

; Logarithm
(define (log m q) ; log base m of q
        (if (= m q) 1
            (+ 1 (log m (/ q m)))
        )
)

; Factorial
(define (fact a) ; returns a!
        (if (= a 0) 1
            (* a (fact (- a 1)))
        )
)

; Choose (probability)
(define (comb n k) ; returns n choose k
        (/ (fact n) (* (fact k) (fact (- n k))))
)

; Square
(define (square x) (* x x))

; Fibonacci
; x=2 iterations ago
; y=1 iteration ago
; z=# of iterations to go
(define (fibhelper x y z)
        (if (= z 0) (+ x y) 
            (fibhelper y (+ x y) (- z 1))
        )
)
; returns fib number at nth position
(define (fib n) (fibhelper 1 0 n))

; Even?
(define (even? x) (= (modulo x 2) 0))

; Odd?
(define (odd? x) (not (even? x)))
