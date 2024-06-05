;EXERCISE 3
;function filter, that filters the elements of x that when applied to f return #t
(define (filter f x)
	(cond
		((null? x) ())
		((f (car x)) (cons (car x) (filter f (cdr x))))
		(else (filter f (cdr x)))
	)
)

;tail-recursive version of filter
(define (filter f x)
	(letrec
		((rec
			(lambda (f x y)
				(cond
					((null? x) y)
					((f (car x)) (rec f (cdr x) (cons (car x) y)) )
					(else (rec f (cdr x) y))
				)
			)
		))
		(rec f x ())
	)
)

;EXERCISE 4 
;allready tail-recursive
(define (reducel f v lst)
	(cond
		((null? (cdr lst)) (f v (car lst)))
		(else (reducel f (f v (car lst)) (cdr lst)))
	)
)

;EXERCISE 5
(define (reducer f lst v)
	(cond
		((null? (cdr lst)) (f (car lst) v))
		(else (f (car lst) (reducer f (cdr lst) v)))
	)
)

;tail-recursive version of reducer
;???

;EXCERCISE 6
(define (filter f x)
	(reducer 
		(lambda (x y)
			(cond
				((f x) (cons x y))
				(else y)
			)
		) x ()
	)
)

;EXERCISE 7
(define (vecadd la lb)
	(map + la lb)
)

(define (vecdot la lb)
	(reducel + 0 (map * la lb))
)

