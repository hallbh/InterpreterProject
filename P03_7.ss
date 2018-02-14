;Benjamin Hall
;Project part 1

;Lex

;check this out: https://www.youtube.com/watch?v=gsTPBPCAW0s
(load "pmatch.ss")
(load "parenthec.ss")

(define-registers expr env k)

;Environment constructors and application
(define-union envr 
	(empty-env)
	(extend-env a^ env^))

(define apply-env 
	(lambda (env y k^)
		(union-case env envr
			[(empty-env) (errorf 'value-of "unbound identifier")]
			[(extend-env a^ env^) (if (zero? y)
									(let* ((k k^) (v a^)) (apply-k k v))
									(let* ((env env^) (y (sub1 y)) (k k^)) (apply-env env y k)))])))

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
	(lambda (k^ v)
		(union-case k kt
			[(empty-k) v]
			[(mult-inner-k v^ k) (let* ((v (* v^ v))) (apply-k k v))]
			[(mult-outer-k nexp2 env k) (begin (set! expr nexp2) 
											   (let* ((v^ v))
											   (set! k (kt_mult-inner-k v^ k))) 
											   (value-of-cps))]
			[(sub-k k) (let* ((v (sub1 v))) (apply-k k v))]
			[(zero-k k) (let* ((v (zero? v))) (apply-k k v))]
			[(if-k conseq alt env k) (if v 
										(begin (set! expr conseq) (value-of-cps))
										(begin (set! expr alt) (value-of-cps)))]
			[(throw-k v^ env^) (begin (set! expr v^)
									  (set! env env^);;;setting env
									  (set! k v) 
									  (value-of-cps))]
			[(let-k b env k) (begin (set! expr b)
									(let* ((a^ v) (env^ env))
									(set! env (envr_extend-env a^ env^))) ;;;;;;;setting env
									(value-of-cps))]
			[(apply-inner-k c^ k^) (let* ((c c^)
										  (a v)
										  (k k^)) (apply-closure c a k))]
			[(apply-outer-k rand env k) (begin (set! expr rand)
											   (let* ((c^ v) (k^ k))
											   (set! k (kt_apply-inner-k c^ k^))) 
											   (value-of-cps))])))

;Closure construction and application
(define-union clos 
	(closure b env))

(define apply-closure 
	(lambda (c a k)
		(union-case c clos 
			[(closure b env) (begin (set! expr b)
									(let* ((a^ a) (env^ env))
									(set! env (envr_extend-env a^ env^))) ;;extending env with closure
									(value-of-cps))])))

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
			[(if test conseq alt) (begin (set! expr test)
										 (set! k (kt_if-k conseq alt env k)) 
										 (value-of-cps))]
			[(mult nexp1 nexp2) (begin (set! expr nexp1)
									   (set! k (kt_mult-outer-k nexp2 env k)) 
									   (value-of-cps))]
			[(sub1 nexp) (begin (set! expr nexp)
								(set! k (kt_sub-k k)) 
								(value-of-cps))]
			[(zero nexp) (begin (set! expr nexp)
								(set! k (kt_zero-k k)) 
								(value-of-cps))]
			[(letcc body) (begin (set! expr body)
								 (let* ((a^ k) (env^ env))
								 (set! env (envr_extend-env a^ env^))) ;;extending env with current continuation and env
								 (value-of-cps))]
			[(throw kexp vexp) (begin (set! expr kexp)
									  (let* ((v^ vexp) (env^ env))
									  (set! k (kt_throw-k v^ env^))) 
									  (value-of-cps))]
			[(let exp body) (begin (set! expr exp)
								   (let* ((b body))
								   (set! k (kt_let-k b env k))) 
								   (value-of-cps))]
			[(lambda body) (let* ((b body);;;;;;;;;this line doesn't call valof
								  (v (clos_closure body env))) (apply-k k v))]
			[(app rator rand) (begin (set! expr rator)
									 (set! k (kt_apply-outer-k rand env k)) 
									 (value-of-cps))])))


;; (let ((f (lambda (f) 
;; 				(lambda (n) 
;; 					(if (zero? n) 
;; 						1 
;; 						(* n ((f f) (sub1 n)))))))) 
;;	 (* (letcc k ((f f) (throw k ((f f) 4)))) 5))

(define main 
	(lambda () 
		(begin (set! expr (exprn_mult (exprn_const 6) (exprn_const 6)))
						; (exprn_let 
						; 	(exprn_lambda 
						; 		(exprn_lambda 
						; 			(exprn_if 
						; 				(exprn_zero (exprn_var 0)) 
						; 				(exprn_const 1) 
						; 				(exprn_mult (exprn_var 0) 
						; 							(exprn_app (exprn_app (exprn_var 1) (exprn_var 1)) 
						; 									   (exprn_sub1 (exprn_var 0))))))) 
						; 	(exprn_mult 
						; 		(exprn_letcc 
						; 			(exprn_app 
						; 				(exprn_app (exprn_var 1) (exprn_var 1)) 
						; 				(exprn_throw (exprn_var 0) 
						; 							 (exprn_app (exprn_app (exprn_var 1) (exprn_var 1)) 
						; 							 			(exprn_const 4))))) 
						; 		(exprn_const 5))))
			   (set! env (envr_empty-env)));;initialize env
			   (set! k (kt_empty-k))
		 	   (value-of-cps)))
