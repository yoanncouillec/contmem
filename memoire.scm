(module memoire
   (main main))

;; mem: addr -> value
;; cont: value * mem -> value

(define (update mem addr v)
   (lambda (addr')
      (if (eqv? addr addr') v (mem addr'))))

(define (evaluate e env mem k)
   (match-case e
      ((or (? boolean?) (? number?)) (k e mem))
      ((? symbol?)
       (k (mem (env e)) mem))
      ((if ?c ?e1 ?e2)
       (evaluate c env mem
          (lambda (v mem')
             (if v
                 (evaluate e1 env mem' k)
                 (evaluate e2 env mem' k)))))
      ((begin ?e1)
       (evaluate e1 env mem k))
      ((begin ?e1 . ?e*)
       (evaluate e1 env mem
          (lambda (v mem')
             (evaluate `(begin ,@e*) env mem' k))))
      ((set! ?x ?e)
       (evaluate e env mem
          (lambda (v mem')
             (k v (update mem' (env x) v)))))
      ((let (?x ?e1) ?e2)
       (evaluate e1 env mem
          (lambda (v mem')
             (let ((n (+ 1 (mem' 0))))
                (evaluate e2
                   (update env x n)
                   (update mem' n v)
                   k)))))
      (else
       (error "evaluate" "cannot evaluate" e))))

(define (main args)
   (let loop ()
	(display "# ")
	(evaluate
	 (read)
         (lambda (x) (error "env" "no such binding" x))
         (update
	  (lambda (addr) (error "mem" "no such address" addr))
	  0
	  0)
         (lambda (v mem) (print v)))
	(loop)))
