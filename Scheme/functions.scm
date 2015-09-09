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
(define (filter P L)
        (if (null? L) L
            (if (P (car L))
                (cons (car L) (filter P (cdr L)))
                (filter P (cdr L))
            )
        )
)

; Reject (opposite of filter)
(define (reject P L)
        (if (null? L) L
            (if (P (car L))
                (reject P (cdr L))
                (cons (car L) (reject P (cdr L)))
            )
        )
)

; MergeSort

(define (split L L1 L2)
        (if(null? L) (list L1 L2)
           (if (null? (cdr L1)) (list(cons (car L) L1) L2)
               (split (cddr L) (cons (car L) L1)
                              (cons (cadr L) L2)
               )
           )
        )
)
(define (merge L)
        (if (null? L) L
            (let ((L1 (car L))
                  (L2 (cadr L))
                 )
                 (cond ((null? L1) L2)
                       ((null? L2) L1)
                       ((< (car L1) (car L2)
                        (cons (car L1) (merge (list (cdr L1) L2))))
                       (#t (cons (car L2) (merge (list L1 (cdr L2)))))
                       )
                 )
            )
        )
)
(define (mergesort L)
        (if (or (null? L) (null? (cdr L))) L
            (merge (map mergesort (split L '() '())))
        )
)


; Selection Sort
(define (selection_sort L)
        (cond ((null? L) L)
             (#t cons (smallest L (car L)) (selection_sort (remove L (smallest L (car L))))
             )
        )
)

(define (smallest L small)
        (if (null? L) small
            (if (< (car L) small) (smallest (cdr L) (car L))
                (smallest (cdr L) small))
        )
)

(define (remove L element)
        (if (null? L)
            (if (= (car L) element) (cdr L)
                (cons (car L) (remove (cdr L) element))
            )
        )
)

; Quicksort
(define (quicksort L)
        (if (null? L) L
            ((let (pivot (car L)))
             (append (append (quicksort (filter (<= pivot)(cdr L)))(pivot)
                     (quicksort (filter (> pivot) (cdr L)))
                     )
             )
            )
        )
)

;-------------- Trees ----------------;
; Binary Search Trees

; Better search
(define (member? x T)
        (if (null? T) #f
            (cond ((< x (car T) (member? x (left T)))
                  ((> x (car T) (member? x (right T)))
                  ((= x (car T) #t)
                  (#t #f))))
            )
        )
)

; Insertion
(define (insert x T)
        (if (null? T) (list x nil nil)
            (cond ((< x (car T)) (list (car T)
                                       (insert x (left T))
                                       (right T))
                  )
                  ((> x (car T)) (list (car T)
                                       (left T)
                                       (insert x (right T)))
                  )
            )
        )
)

; Binary Search Tree Left
(define (left T) (cadr T))

; Binary Search Tree Right
(define (right T) (caddr T))

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
