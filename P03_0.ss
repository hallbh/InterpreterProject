;Benjamin Hall
;Project part 1

;Lex

;check this out: https://www.youtube.com/watch?v=gsTPBPCAW0s
(load "pmatch.ss")
(load "parenthec.ss")


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

(define-union exprn 
	(const cexp) 
	(var n) 
	(if test conseq alt) 
	(mult nexp1 nexp2) 
	(sub1 nexp) 
	(zero nexp) 
	(letcc body) 
	(throw kexp vexp) 
	(let exp body) 
	(lambda body) 
	(app rator rand))

(define value-of-cps
	(lambda (expr env k)
		(union-case expr exprn
			[(const cexp) (apply-k k cexp)]
			[(var n) (apply-env env n k)]
			[(if test conseq alt) (value-of-cps test env (if-k conseq alt env k))]
			[(mult nexp1 nexp2) (value-of-cps nexp1 env (mult-outer-k nexp2 env k))]
			[(sub1 nexp) (value-of-cps nexp env (sub-k k))]
			[(zero nexp) (value-of-cps nexp env (zero-k k))]
			[(letcc body) (value-of-cps body (extend-env k env) k)]
			[(throw kexp vexp) (value-of-cps kexp env (throw-k vexp env))]
			[(let exp body) (value-of-cps exp env (let-k body env k))]
			[(lambda body) (apply-k k (make-closure body env))]
			[(app rator rand) (value-of-cps rator env (apply-outer-k rand env k))])))


;; (let ((f (lambda (f) 
;; 				(lambda (n) 
;; 					(if (zero? n) 
;; 						1 
;; 						(* n ((f f) (sub1 n)))))))) 
;;	 (* (letcc k ((f f) (throw k ((f f) 4)))) 5))

(define main 
	(lambda () 
		(value-of-cps 
			(exprn_let 
				(exprn_lambda 
					(exprn_lambda 
						(exprn_if 
							(exprn_zero (exprn_var 0)) 
							(exprn_const 1) 
							(exprn_mult (exprn_var 0) 
										(exprn_app (exprn_app (exprn_var 1) (exprn_var 1)) 
												   (exprn_sub1 (exprn_var 0))))))) 
				(exprn_mult 
					(exprn_letcc 
						(exprn_app 
							(exprn_app (exprn_var 1) (exprn_var 1)) 
							(exprn_throw (exprn_var 0) 
										 (exprn_app (exprn_app (exprn_var 1) (exprn_var 1)) 
										 			(exprn_const 4))))) 
					(exprn_const 5))) 
			(empty-env) 
			(empty-k))))
