(load'"./my_std.scm")

(define swaphead (lambda (x)
        (cons
                (car (cdr x))
                (cons (car x) (cdr (cdr x)))
        )
))

(define (cmp3dif x y)
        (cond ((and (list? x) (list? y))
                (cond
                        ((and (equal? 3 (length x)) (equal? 2 (length y)))
                                (and
                                        (equal? (- (car(cdr x)) (car x)) (car y))
                                        (equal? (- (car(cdr(cdr x))) (car(cdr x))) (car(cdr y)))
                                )
                        )
                        (else #f)
                ))
                (else #f)
        )
)

(define (quadeq a b c)
        (let    ((x (sqrt (- (* b b) (* 4 (* a c)))))
                 (y (* 2 a))
                )
                (cons
                        (/ (- (- 0 b) x) y)
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

(define (concat a b)
	(cond
		((null? a) b)
		(else (cons (car a)(concat (cdr a) b)))
	)
)

(define (subst in out lst)
	(cond
		((equal? lst out) in)
		((not (pair? lst)) lst)
		(else
			(cons
				(subst in out (car lst))
				(subst in out (cdr lst))
			)
		)
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
