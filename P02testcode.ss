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
  (display 'test-value-of-cps)
  (test-value-of-cps))

(define r run-all)
