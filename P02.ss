;Benjamin Hall
;Project part 1

;Lex

;check this out: https://www.youtube.com/watch?v=xPTjlUvjJwE
(load "pmatch.ss")

;I kept lex around so I could make my own test cases. And because he's cute.
;http://bookwormadventures.wikia.com/wiki/File:Lex.png
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

;Environment constructors and application
(define (empty-env) `(empty-env))
(define (extend-env a^ env^) `(extend-env ,a^ ,env^))

(define apply-env 
	(lambda (env y k^)
		(pmatch env
			[(empty-env) (error 'value-of "unbound identifier")]
			[(extend-env ,a^ ,env^) (if (zero? y)
									(apply-k k^ a^)
									(apply-env env^ (sub1 y) k^))])))

;Continuation constructors and application
(define (empty-k) `(empty-k))
(define (mult-inner-k v^ k) `(mult-inner-k ,v^ ,k))
(define (mult-outer-k x2 env k) `(mult-outer-k ,x2 ,env ,k))
(define (sub-k k) `(sub-k ,k))
(define (zero-k k) `(zero-k ,k))
(define (if-k conseq alt env k) `(if-k ,conseq ,alt ,env ,k))
(define (throw-k v^ env^) `(throw-k ,v^ ,env^))
(define (let-k b env k) `(let-k ,b ,env ,k))
(define (apply-inner-k c^ k^) `(apply-inner-k ,c^ ,k^))
(define (apply-outer-k rand env k) `(apply-outer-k ,rand ,env ,k))

(define apply-k 
	(lambda (k v)
		(pmatch k
			[(empty-k) v]
			[(mult-inner-k ,v^ ,k) (apply-k k (* v^ v))]
			[(mult-outer-k ,x2 ,env ,k) (value-of-cps x2 env (mult-inner-k v k))]
			[(sub-k ,k) (apply-k k (sub1 v))]
			[(zero-k ,k) (apply-k k (zero? v))]
			[(if-k ,conseq ,alt ,env ,k) (if v (value-of-cps conseq env k)
											   (value-of-cps alt env k))]
			[(throw-k ,v^ ,env^) (value-of-cps v^ env^ v)]
			[(let-k ,b ,env ,k) (value-of-cps b (extend-env v env) k)]
			[(apply-inner-k ,c^ ,k^) (apply-closure c^ v k^)]
			[(apply-outer-k ,rand ,env ,k) (value-of-cps rand env (apply-inner-k v k))])))

;Closure construction and application
(define (make-closure b env) `(closure ,b ,env))

(define apply-closure 
	(lambda (c a k)
		(pmatch c 
			[(closure ,b ,env) (value-of-cps b (extend-env a env) k)])))

(define value-of-cps
	(lambda (expr env k)
		(pmatch expr
			[(const ,expr) (apply-k k expr)]
			[(var ,expr) (apply-env env expr k)]
			[(mult ,x1 ,x2) (value-of-cps x1 env (mult-outer-k x2 env k))]
			[(sub1 ,x) (value-of-cps x env (sub-k k))]
			[(zero ,x) (value-of-cps x env (zero-k k))]
			[(if ,test ,conseq ,alt) (value-of-cps test env (if-k conseq alt env k))]
			[(letcc ,body) (value-of-cps body (extend-env k env) k)]
			[(throw ,kexp ,vexp) (value-of-cps kexp env (throw-k vexp env))]
			[(let ,e ,body) (value-of-cps e env (let-k body env k))]
			[(lambda ,body) (apply-k k (make-closure body env))]
			[(app ,rator ,rand) (value-of-cps rator env (apply-outer-k rand env k))])))