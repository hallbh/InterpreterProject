;Benjamin Hall
;Project part 3

(load "pmatch.ss")
(load "parenthec.ss")

(define-registers a c env expr k v y)
(define-program-counter pc)

;Environment constructors and application
(define-union envr 
	(empty-env)
	(extend-env a^ env^))

(define-label apply-env 
	(union-case env envr
		[(empty-env) (errorf 'value-of "unbound identifier")]
		[(extend-env a^ env^) (if (zero? y)
								(begin (set! v a^) 
									   (set! pc apply-k))
								(begin (set! env env^) 
									   (set! y (sub1 y)) 
									   (set! pc apply-env)))]))

;Continuation constructors and application
(define-union kt
	(empty-k dismount)
	(mult-inner-k v^ k^)
	(mult-outer-k nexp2 env k^)
	(sub-k k^)
	(zero-k k^)
	(if-k conseq alt env k^)
	(throw-k v^ env^)
	(let-k b env k^)
	(apply-inner-k c^ k^)
	(apply-outer-k rand env k^))

(define-label apply-k 
	(union-case k kt
		[(empty-k dismount) (dismount-trampoline dismount)]
		[(mult-inner-k v^ k^) (begin (set! v (* v^ v)) 
									 (set! k k^)
									 (set! pc apply-k))]
		[(mult-outer-k nexp2 env k^) (begin (set! expr nexp2) 
										    (set! k (kt_mult-inner-k v k^))
										    (set! pc value-of-cps))]
		[(sub-k k^) (begin (set! v (sub1 v))
						   (set! k k^)
						   (set! pc apply-k))]
		[(zero-k k^) (begin (set! v (zero? v))
							(set! k k^)
						    (set! pc apply-k))]
		[(if-k conseq alt env^ k^) (if v 
									(begin (set! expr conseq)
										   (set! env env^)
										   (set! k k^)
										   (set! pc value-of-cps))
									(begin (set! expr alt) 
										   (set! env env^)
										   (set! k k^)
										   (set! pc value-of-cps)))]
		[(throw-k v^ env^) (begin (set! expr v^)
								  (set! env env^)
								  (set! k v)
								  (set! pc value-of-cps))]
		[(let-k b env^ k^) (begin (set! expr b)
								  (set! env (envr_extend-env v env^))
								  (set! k k^) 
								  (set! pc value-of-cps))]
		[(apply-inner-k c^ k^) (begin (set! c c^)
									  (set! a v)
									  (set! k k^) 
									  (set! pc apply-closure))]
		[(apply-outer-k rand env^ k^) (begin (set! expr rand)
											 (set! env env^)
										     (set! k (kt_apply-inner-k v k^)) 
										     (set! pc value-of-cps))]))

;Closure construction and application
(define-union clos 
	(closure b env))

(define-label apply-closure
	(union-case c clos 
		[(closure b env^) (begin (set! expr b)
								 (set! env (envr_extend-env a env^)) 
								 (set! pc value-of-cps))]))

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

(define-label value-of-cps
	(union-case expr exprn
		[(const cexp) (begin (set! v cexp)
						     (set! pc apply-k))]
		[(var n) (begin (set! k^ k) 
						(set! y n) 
						(set! pc apply-env))]
		[(if test conseq alt) (begin (set! expr test)
									 (set! k (kt_if-k conseq alt env k)) 
									 (set! pc value-of-cps))]
		[(mult nexp1 nexp2) (begin (set! expr nexp1)
								   (set! k (kt_mult-outer-k nexp2 env k)) 
								   (set! pc value-of-cps))]
		[(sub1 nexp) (begin (set! expr nexp)
							(set! k (kt_sub-k k))
							(set! pc value-of-cps))]
		[(zero nexp) (begin (set! expr nexp)
							(set! k (kt_zero-k k))
							(set! pc value-of-cps))]
		[(letcc body) (begin (set! expr body)
							 (set! env (envr_extend-env k env)) 
							 (set! pc value-of-cps))]
		[(throw kexp vexp) (begin (set! expr kexp)
								  (set! k (kt_throw-k vexp env)) 
								  (set! pc value-of-cps))]
		[(let exp body) (begin (set! expr exp)
							   (set! k (kt_let-k body env k)) 
							   (set! pc value-of-cps))]
		[(lambda body) (begin (set! b body)
							  (set! v (clos_closure body env))
							  (set! pc apply-k))]
		[(app rator rand) (begin (set! expr rator)
								 (set! k (kt_apply-outer-k rand env k)) 
								 (set! pc value-of-cps))]))


;; (let ((f (lambda (f) 
;; 				(lambda (n) 
;; 					(if (zero? n) 
;; 						1 
;; 						(* n ((f f) (sub1 n)))))))) 
;;	 (* (letcc k ((f f) (throw k ((f f) 4)))) 5))

(define-label main 
	(begin (set! expr 
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
						(exprn_const 5))))
		   (set! env (envr_empty-env))
		   (set! pc value-of-cps)
		   (mount-trampoline kt_empty-k k pc)
		   (printf "Fact 5: ~d\n" v)))
