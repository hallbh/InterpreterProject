;Benjamin Hall
;Project part 1

;Lex

;check this out: https://www.youtube.com/watch?v=xPTjlUvjJwE
(load "pmatch.ss")

(define lex
	(lambda (exp acc)
		(pmatch exp
			[,y (guard (symbol? y)) `(var ,(list-index-of-eqv? y acc))]
			[,n (guard (number? n)) `(const ,n)]
			[(zero? ,body) 			`(zero ,(lex body acc))]
			[(sub1 ,nexp) 			`(sub1 ,(lex nexp acc))]
			[(* ,x ,y) 				`(mult ,(lex x acc) ,(lex y acc))]
			[(if ,c ,t ,f) 			`(if ,(lex c acc) 
										 ,(lex t acc) 
										 ,(lex f acc))]
			[(let ((,x ,e)) ,body)  `(let ,(lex e (cons x acc)) ,(lex body (cons x acc)))]
			[(lambda (,x) ,body) 	`(lambda ,(lex body (cons x acc)))]
			[(let/cc ,x ,b) 		`(letcc ,(lex b (cons x acc)))]
			[(throw ,kexp ,vexp)    `(throw ,(lex kexp acc) ,(lex vexp acc))]
			[(,rator ,rand) 		`(app ,(lex rator acc) ,(lex rand acc))])))

(define list-index-of-eqv?
	(lambda (x ls)
		(cond
			((eqv? x (car ls)) 0)
			(else (add1 (list-index-of-eqv? x (cdr ls)))))))


;CPS the interpreter here we go
(define value-of-cps
	(lambda (expr env-cps k-cps)
		(pmatch expr
			[(const ,expr) (k-cps expr)]
			[(var ,expr) (env-cps expr k-cps)]
			[(mult ,x1 ,x2) (value-of-cps x1 env-cps (lambda (v) 
								(value-of-cps x2 env-cps (lambda (w) 
									(k-cps (* v w))))))]
			[(sub1 ,x) (value-of-cps x env-cps (lambda (v) (k-cps (sub1 v))))]
			[(zero ,x) (value-of-cps x env-cps (lambda (v) (k-cps (zero? v))))]
			[(if ,test ,conseq ,alt) (value-of-cps test env-cps (lambda (v) (if v
								     								(value-of-cps conseq env-cps k-cps)
	                     					     					(value-of-cps alt env-cps k-cps))))]
			[(letcc ,body) (value-of-cps body (lambda (y k^) (if (zero? y)
																  (k^ k-cps)
																  (env-cps (sub1 y) k^))) k-cps)]
			[(throw ,kexp ,vexp) (value-of-cps kexp env-cps (lambda (k) (value-of-cps vexp env-cps k)))]
			[(let ,e ,body) (value-of-cps e env-cps (lambda (a) (value-of-cps body (lambda (y k-cps) (if (zero? y)
																								  (k-cps a)
																								  (env-cps (sub1 y) k-cps))) k-cps)))]
			[(lambda ,body) (k-cps (lambda (a k-cps) (value-of-cps body (lambda (y k-cps) (if (zero? y)
																				  (k-cps a)
																				  (env-cps (sub1 y) k-cps))) k-cps)))]
			[(app ,rator ,rand) (value-of-cps rator env-cps (lambda (c) (value-of-cps rand env-cps (lambda (a) (c a k-cps)))))])))
			
(define empty-env
	(lambda ()
		(lambda (y)
			(error 'value-of "unbound identifier"))))

(define empty-k
	(lambda ()
		(lambda (v)
			v)))
