
(define (test-lex)
   (let ([correct '((lambda (var 0)) (lambda (const 120))
		    (lambda (lambda (var 1)))
		    (app (lambda (var 0)) (const 5))
		    (app (lambda (const 0)) (const 5))
		    (lambda (lambda (app (var 0) (var 1))))
		    (lambda (lambda (app (var 0) (var 0))))
		    (lambda (app (lambda (app (var 0) (var 1)))
			     (lambda (lambda (app (var 2) (var 1))))))
		    (letcc (const 5)) (letcc (throw (var 0) (const 5)))
		    (letcc (throw (var 0) (mult (const 5) (const 5))))
		    (letcc
		      (throw
			(var 0)
			(app (app (lambda (var 0)) (var 0))
			     (mult (const 5) (const 5)))))
		    (letcc (sub1 (throw (var 0) (const 5))))
		    (letcc (throw (throw (var 0) (const 5)) (const 6)))
		    (letcc (throw (const 5) (throw (var 0) (const 5))))
		    (mult
		      (const 3)
		      (letcc (throw (const 5) (throw (var 0) (const 5)))))
		    (lambda (lambda
			     (lambda (lambda
				      (lambda (lambda
					       (lambda (lambda
							(app (app (app (app (app (var 1)
										 (var 3))
									    (var 5))
								       (var 2))
								  (var 0))
							     (var 1))))))))))
		    (lambda (lambda
			     (lambda (lambda
				      (lambda (lambda
					       (app (lambda (lambda
							     (lambda (app
								      (app (app (app (app (var 2)
											  (var 1))
										     (var 0))
										(var 5))
									   (var 4))
								      (var 3)))))
						    (lambda (lambda
							     (lambda (app
								      (app (app (app (app (var 8)
											  (var 7))
										     (var 6))
										(var 2))
									   (var 1))
								      (var 0))))))))))))
		    (app (lambda (var 0)) (const 5))
		    (lambda (lambda
			     (if (zero (var 0))
				 (const 1)
				 (mult
				   (var 0)
				   (app (var 1) (sub1 (var 0)))))))
		    (let (lambda
			  [lambda (if (zero (var 0))
				      (const 1)
				      (mult
					(var 0)
					(app (app (var 1) (var 1))
					     (sub1 (var 0)))))])
		      (app (app (var 0) (var 0)) (const 5))))]
	 [answers (list 
		   (lex '(lambda (x) x) '())
		   (lex '(lambda (x) 120) '())
		   (lex '(lambda (y) (lambda (x) y)) '())
		   (lex '((lambda (x) x) 5) '())
		   (lex '((lambda (x) 0) 5) '())
		   (lex '(lambda (y) (lambda (x) (x y))) '())
		   (lex '(lambda (x) (lambda (x) (x x))) '())
		   (lex '(lambda (y)
			   ((lambda (x) (x y))
			     (lambda (c) (lambda (d) (y c)))))
			'())
		   (lex '(let/cc k 5) '())
		   (lex '(let/cc k (throw k 5)) '())
		   (lex '(let/cc k (throw k (* 5 5))) '())
		   (lex '(let/cc k (throw k (((lambda (x) x) k) (* 5 5))))
			'())
		   (lex '(let/cc k (sub1 (throw k 5))) '())
		   (lex '(let/cc k (throw (throw k 5) 6)) '())
		   (lex '(let/cc k (throw 5 (throw k 5))) '())
		   (lex '(* 3 (let/cc k (throw 5 (throw k 5)))) '())
		   (lex '(lambda (a)
			   (lambda (b)
			     (lambda (c)
			       (lambda (a)
				 (lambda (b)
				   (lambda (d)
				     (lambda (a)
				       (lambda (e)
					 (((((a b) c) d) e) a)))))))))
			'())
		   (lex '(lambda (a)
			   (lambda (b)
			     (lambda (c)
			       (lambda (w)
				 (lambda (x)
				   (lambda (y)
				     ((lambda (a)
					(lambda (b)
					  (lambda (c)
					    (((((a b) c) w) x) y))))
				       (lambda (w)
					 (lambda (x)
					   (lambda (y)
					     (((((a b) c) w) x)
					       y)))))))))))
			'())
		   (lex '((lambda (x) x) 5) '())
		   (lex '(lambda (!)
			   (lambda (n)
			     (if (zero? n) 1 (* n (! (sub1 n))))))
			'())
		   (lex '(let ([! (lambda (!)
				    (lambda (n)
				      (if (zero? n)
					  1
					  (* n ((! !) (sub1 n))))))])
			   ((! !) 5))
			'()))])
     (display-results correct answers equal?)))

 (define (test-value-of-cps)
  (let ([correct '(5 25 #f 4 3 #f 4 4 5 5 6 5 5 4 5 25 5 3 5 5 25 25 5 5 5 15 4 4 1)]
	[answers (list (value-of-cps '(const 5) (empty-env) (empty-k))
		   (value-of-cps
		     '(mult (const 5) (const 5))
		     (empty-env)
		     (empty-k))
		   (value-of-cps '(zero (const 5)) (empty-env) (empty-k))
		   (value-of-cps '(sub1 (const 5)) (empty-env) (empty-k))
		   (value-of-cps
		     '(sub1 (sub1 (const 5)))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(zero (sub1 (const 6)))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(if (zero (const 5))
			  (const 3)
			  (mult (const 2) (const 2)))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		    '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3))
		    (empty-env)
		    (empty-k))
		   (value-of-cps
		     '(app (lambda (const 5)) (const 6))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(app (lambda (var 0)) (const 5))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(app (app (lambda (lambda (var 1))) (const 6)) (const 5))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(app (lambda (app (lambda (var 1)) (const 6))) (const 5))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(app (lambda (if (zero (var 0)) (const 4) (const 5)))
			   (const 3))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(let (const 6) (const 4))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(let (const 5) (var 0))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(mult (const 5) (let (const 5) (var 0)))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(app (if (zero (const 4))
			       (lambda (var 0))
			       (lambda (const 5)))
			   (const 3))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(app (if (zero (const 0))
			       (lambda (var 0))
			       (lambda (const 5)))
			   (const 3))
		     (empty-env)
		     (empty-k))
		   (value-of-cps '(letcc (const 5)) (empty-env) (empty-k))
		   (value-of-cps
		     '(letcc (throw (var 0) (const 5)))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(letcc (throw (var 0) (mult (const 5) (const 5))))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(letcc
			(throw
			  (app (lambda (var 0)) (var 0))
			  (mult (const 5) (const 5))))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(letcc (sub1 (throw (var 0) (const 5))))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(letcc (throw (throw (var 0) (const 5)) (const 6)))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(letcc (throw (const 5) (throw (var 0) (const 5))))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(mult
			(const 3)
			(letcc (throw (const 5) (throw (var 0) (const 5)))))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(if (zero (const 5))
			  (app (lambda (app (var 0) (var 0)))
			       (lambda (app (var 0) (var 0))))
			  (const 4))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(if (zero (const 0))
			  (const 4)
			  (app (lambda (app (var 0) (var 0)))
			       (lambda (app (var 0) (var 0)))))
		     (empty-env)
		     (empty-k))
		   (value-of-cps
		     '(app (lambda (app (app (var 0) (var 0)) (const 2)))
			   (lambda (lambda
				    (if (zero (var 0))
					(const 1)
					(app (app (var 1) (var 1))
					     (sub1 (var 0)))))))
		     (empty-env)
		     (empty-k)))])
		    (display-results correct answers equal?)))

;;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))

(define set-equals?  ; are these list-of-symbols equal when
  (lambda (s1 s2)    ; treated as sets?
    (if (or (not (list? s1)) (not (list? s2)))
        #f
        (not (not (and (is-a-subset? s1 s2) (is-a-subset? s2 s1)))))))

(define is-a-subset?
  (lambda (s1 s2)
    (andmap (lambda (x) (member x s2))
      s1)))

;; You can run the tests individually, or run them all
;; by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'test-lex)
  (test-lex)
  (display 'test-value-of-cps)
  (test-value-of-cps))

(define r run-all)
