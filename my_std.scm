(define (length lst)
        (if (null? lst) 0
                (+ 1 (length (cdr lst)))
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

(define (iter fnc lst)
	(cond
		((null? lst) ())
		(else (cons (fnc (car lst)) (iter fnc (cdr lst))))
	)
)

(define (all fnc lst)
	(cond
		((null? lst) #t)
		((boolean? (fnc (car lst))) (and (fnc (car lst)) (all fnc (cdr lst))))
		(else #f)
	)
)

(define (any fnc lst)
	(cond
		((null? lst) #f)
		((boolean? (fnc (car lst))) (or (fnc (car lst)) (any fnc (cdr lst))))
		(else #f)
	)
)

(define (sorted lst)
	(cond
		((null? lst) #f)
		((null? (cdr lst)) #t)
		((not (number? (car lst))) #f)
		((not (number? (cadr lst))) #f)
		((and (<= (car lst) (cadr lst)) (sorted (cdr lst))) #t)
		(else #f)
	)
)

(define (bsort lst)
	(cond
		((sorted lst) lst)
		((> (car lst) (cadr lst))
			(bsort (cons (cadr lst)
				(bsort (cons (car lst) (cddr lst)))
			))
		)
		(else (bsort (cons (car lst)(bsort (cdr lst)))))
	)
)

(define (zip a b)
        (cond
                ((null? a) b)
                ((null? b) a)
                (else (cons
                        (cons (car a) (car b))
                        (zip (cdr a) (cdr b))
                ))
        )
)
