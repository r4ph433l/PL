                        (/ (+ (- 0 b) x) y)
                )
        )
)

(define (quadeqpq a b c)
        (let    ((p (/ b a))
                 (x (sqrt (- (/ (* (/ b a) (/ b a))(* 2 2))(/ c a))))
                )
                (cons
                        (- (/ (- 0 p) 2) x)
                        (+ (/ (- 0 p) 2) x)
                )
        )
)

(define (countin lst e)
        (cond
                ((null? lst) 0)
                ((equal? car(lst) e) (+ 1 (countin cdr(lst) e)))
                (else (countin cdr(lst) e))
        )
)


(define (countin lst e)
        (cond
                ((null? lst) 0)
                ((equal? (car lst) e) (+ 1 (countin (cdr lst) e)))
                (else (countin (cdr lst) e))
        )
)

(define (erase lst e)
        (cond
                ((null? lst) ())
                ((equal? (car lst) e) (erase(cdr lst) e))
                (else (cons (car lst)(erase (cdr lst) e)))
        )
)

(define (at lst i)
        (cond
                ((equal? i 0) (car lst))
                (else (at (cdr lst) (- i 1)))
        )
)
