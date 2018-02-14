;Benjamin Hall
;Project part 1

;Lex

;check this out: https://www.youtube.com/watch?v=gsTPBPCAW0s
(load "pmatch.ss")
(load "parenthec.ss")


;Environment constructors and application
(define-union envr 
	(empty-env)
	(extend-env a^ env^))

(define apply-env 
	(lambda (env y k^)
		(union-case env envr
			[(empty-env) (error 'value-of "unbound identifier")]
			[(extend-env a^ env^) (if (zero? y)
									(apply-k k^ a^)
									(apply-env env^ (sub1 y) k^))])))

;Continuation constructors and application
(define-union kt
	(empty-k)
	(mult-inner-k v^ k)
	(mult-outer-k nexp2 env k)
	(sub-k k)
	(zero-k k)
	(if-k conseq alt env k)
	(throw-k v^ env^)
	(let-k b env k)
	(apply-inner-k c^ k^)
	(apply-outer-k rand env k))

(define apply-k 
	(lambda (k v)
		(union-case k kt
			[(empty-k) v]
			[(mult-inner-k v^ k) (let* ((v (* v^ v))) (apply-k k v))]
			[(mult-outer-k nexp2 env k) (let* ((expr nexp2) 
											   (v^ v)
											   (k (kt_mult-inner-k v^ k))) (value-of-cps expr env k))]
			[(sub-k k) (let* ((v (sub1 v))) (apply-k k v))]
			[(zero-k k) (let* ((v (zero? v))) (apply-k k v))]
			[(if-k conseq alt env k) (if v 
										(let* ((expr conseq)) (value-of-cps expr env k))
										(let* ((expr alt)) (value-of-cps expr env k)))]
			[(throw-k v^ env^) (let* ((expr v^)
									  (env env^)
									  (k v)) (value-of-cps expr env k))]
			[(let-k b env k) (let* ((expr b)
									(a^ v)
									(env^ env)
									(env (envr_extend-env a^ env^))) 
									(value-of-cps expr env k))]
			[(apply-inner-k c^ k^) (let* ((c c^)
										  (a v)
										  (k k^)) (apply-closure c a k))]
			[(apply-outer-k rand env k) (let* ((expr rand)
											   (c^ v)
											   (k^ k)
											   (k (kt_apply-inner-k c^ k^))) (value-of-cps rand env k))])))

;Closure construction and application
(define-union clos 
	(closure b env))

(define apply-closure 
	(lambda (c a k)
		(union-case c clos 
			[(closure b env) (value-of-cps b (envr_extend-env a env) k)])))

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
			[(if test conseq alt) (value-of-cps test env (kt_if-k conseq alt env k))]
			[(mult nexp1 nexp2) (value-of-cps nexp1 env (kt_mult-outer-k nexp2 env k))]
			[(sub1 nexp) (value-of-cps nexp env (kt_sub-k k))]
			[(zero nexp) (value-of-cps nexp env (kt_zero-k k))]
			[(letcc body) (value-of-cps body (envr_extend-env k env) k)]
			[(throw kexp vexp) (value-of-cps kexp env (kt_throw-k vexp env))]
			[(let exp body) (value-of-cps exp env (kt_let-k body env k))]
			[(lambda body) (apply-k k (clos_closure body env))]
			[(app rator rand) (value-of-cps rator env (kt_apply-outer-k rand env k))])))


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
			(envr_empty-env) 
			(kt_empty-k))))
