(load'"./my_std.scm")
(define (countin lst e)
	(letrec
		((rec
			(lambda (lst e c)
				(cond
					((null? lst) c)
					((equal? (car lst) e) (rec (cdr lst) e (+ c 1)))
					(else (rec (cdr lst) e c))
				)
			)
		))
		(rec lst e 0)
	)
)
