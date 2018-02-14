;Benjamin Hall
;Project part 1

;Lex

;check this out: https://www.youtube.com/watch?v=gsTPBPCAW0s
(load "pmatch.ss")
(load "parenthec.ss")

(define-registers env y k^ k v c a expr)

;Environment constructors and application
(define-union envr 
	(empty-env)
	(extend-env a^ env^))

(define apply-env 
	(lambda ()
		(union-case env envr
			[(empty-env) (error 'value-of "unbound identifier")]
			[(extend-env a^ env^) (if (zero? y)
									(begin (set! k k^) ;;realistically this should result in no change
										   (set! v a^) ;;give apply-k the right argument
										   (apply-k)) ;;if it's what we want apply the continuation 
									(begin (set! env env^) ;;shrink the environment as we look deeper
										   (set! y (sub1 y)) ;;<><><><>THIS IS WHERE I STOPPED<><><><>
										   (set! k k^) 
										   (apply-env)))]))) ;;recurse down

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
	(lambda ()
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
	(lambda ()
		(union-case c clos 
			[(closure b env) (let* ((expr b)
									(a^ a)
									(env^ env)
									(env (envr_extend-env a^ env^))) (value-of-cps expr env k))])))

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
	(lambda ()
		(union-case expr exprn
			[(const cexp) (let* ((v cexp)) (apply-k k v))]
			[(var n) (let* ((k^ k) (y n)) (apply-env env y k^))]
			[(if test conseq alt) (let* ((expr test)
										 (k (kt_if-k conseq alt env k))) (value-of-cps expr env k))]
			[(mult nexp1 nexp2) (let* ((expr nexp1)
									   (k (kt_mult-outer-k nexp2 env k))) (value-of-cps expr env k))]
			[(sub1 nexp) (let* ((expr nexp)
								(k (kt_sub-k k))) (value-of-cps expr env k))]
			[(zero nexp) (let* ((expr nexp)
								(k (kt_zero-k k))) (value-of-cps expr env k))]
			[(letcc body) (let* ((expr body)
								 (a^ k)
								 (env^ env)
								 (env (envr_extend-env a^ env^))) (value-of-cps expr env k))]
			[(throw kexp vexp) (let* ((expr kexp)
									  (v^ vexp)
									  (env^ env)
									  (k (kt_throw-k v^ env^))) (value-of-cps expr env k))]
			[(let exp body) (let* ((expr exp)
								   (b body)
								   (k (kt_let-k b env k))) (value-of-cps expr env k))]
			[(lambda body) (let* ((b body)
								  (v (clos_closure body env))) (apply-k k v))]
			[(app rator rand) (let* ((expr rator)
									 (k (kt_apply-outer-k rand env k))) (value-of-cps expr env k))])))


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
