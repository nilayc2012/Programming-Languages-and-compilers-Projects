
;;---------------------------------------------------------------------
;;
;;  C A L L -- B Y -- V A L U E   ,    S T A T I C    S C O P I N G
;;
;;                       Nilay Chakraborty
;;---------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(define name?
  (lambda (s)
    (and (symbol? s)
         (not (memq s '(if lambda let let*))))))

(define andmap
  (lambda (f l)
    (if (null? l) 
	#t
	(and (f (car l))
	     (andmap f (cdr l))))))

(define parse
  (lambda (m)
    (cond
      ((number? m)  `(&const ,m))
      ((eq? #t m)   `(&const #t))
      ((eq? #f m)   `(&const #f))
      ((name? m)    `(&var ,m))
      ((pair? m)    (cond 
                      ((eq? `if (car m))
                        (if (= 4 (length m))
                            `(&if ,(parse (cadr m))
                                  ,(parse (caddr m)) ,(parse (cadddr m)))
                            (error 'parse "Syntax error")))
                      ((eq? `lambda (car m))
                        (if (and (= 3 (length m))
                                 (list? (cadr m))
                                 (andmap name? (cadr m)))
                            `(&lambda ,(cadr m) ,(parse (caddr m)))
                            (error 'parse "Syntax error")))
		((eq? `let (car m))
                        (if (and (= 3 (length m))
                                 (list? (cadr m))
                                 (andmap name-check? (cadr m)))
                            `(&let,(map let-val-parse (cadr m)),(parse (caddr m)))
                            (error 'parse "Syntax error")))
                ((eq? `let* (car m))
                        (if (and (= 3 (length m))
                                 (list? (cadr m))
                                 (andmap name-check? (cadr m)))
                            `(&let*,(map let-val-parse (cadr m)),(parse (caddr m)))
                            (error 'parse "Syntax error")))
                      (else
                        `(&apply ,(parse (car m)) ,(parse* (cdr m))))))
      (else         (error 'parse "Syntax error")))))

(define parse* (lambda (m) (map parse m)))

(define name-check?
  (lambda (m)
    (name? (car m))))

(define let-val-parse
  (lambda (m)
   (if (= (length m) 2) (list (car m) (parse (cadr m))) (error 'parse "Syntax error") )))

(define let-val-unparse
  (lambda (m)
   (if (= (length m) 2) (list (car m) (unparse (cadr m))) (error 'parse "Syntax error") )))

(define unparse
  (lambda (a)
    (cond
      ((eq? (car a) `&const) (cadr a))
      ((eq? (car a) `&var)   (cadr a))
      ((eq? (car a) `&if)    `(if ,(unparse (cadr a)) ,(unparse (caddr a)) 
                                 ,(unparse (cadddr a))))
      ((eq? (car a) '&lambda) `(lambda ,(cadr a) ,(unparse (caddr a))))
      ((eq? (car a) '&let) `(let ,(map let-val-unparse (cadr a)) ,(unparse (caddr a))))
       ((eq? (car a) '&let*) `(let ,(map let-val-unparse (cadr a)) ,(unparse (caddr a))))
      ((eq? (car a) `&apply)  (cons (unparse (cadr a)) (map unparse (caddr a))))
      ((eq? (car a) '&closure) `(lambda ,(caddr a) ,(unparse (cadddr a))))
      (else  (error 'unparse "unexpected syntax tree" a)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Environment and closure representations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; environments, functional implementation
(define extend*
  (lambda (env xs vs)
    (if (null? xs)
        env
        (extend* (extend env (car xs) (car vs)) (cdr xs) (cdr vs)))))

(define extend
  (lambda (env x v)
    (append (list (list x v)) env )
    ))

(define interpret-free-var
  (lambda (x)
    `(&const ,(eval x))))

(define empty-env `())

(define lookup
  (lambda (env y) (cond ((null? env) (if (memq y '(+ - * /)) (interpret-free-var y) (error y "undefined variable")) )
                        ((eq? (car(car env)) y) (cadr(car env)))
                        (else (lookup (cdr env) y)))

                        ))

  
;; closures, list implementation 
(define mk-closure                     ;; returns (&closure env parm-list body)
  (lambda (env v)
    (cond
     ((eq? (car v) '&lambda) `(&closure ,env ,(cadr v) ,(caddr v)))))) 

(define apply-cl
  (lambda (vf va)
    (cond
     ((eq? (car vf) '&closure)
      (let ((env (cadr vf))      ;; environment
	    (p   (caddr vf))     ;; parameter list
	    (b   (cadddr vf)))   ;; body
        (if (= (length p) (length va))
            (ev b (extend* env p va))
            (error 'apply-cl "wrong number of arguments")))))))

(define delta
  (lambda (f a)
    (let ((R (lambda (s) `(&const ,s)))
          (R-1 (lambda (cl)
                 (cond
		  ((eq? (car cl) `&const) (cadr cl))
		  (else (error 'delta "non-constant argument"))))))
      (R (apply (R-1 f) (map R-1 a))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Closure Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ev
  (lambda (e env)
    (cond
      ((eq? (car e) '&const) e)                            ;; e == (&const c)
      ((eq? (car e) '&var) (lookup env (cadr e)))          ;; e == (&var v)
      ((eq? (car e) '&lambda)  (mk-closure env e))         ;; e == (&lambda parm-list body)
      ((eq? (car e) '&if) (let ((a (cadr e))               ;; e == (&if a b c)
				(b (caddr e))
				(c (cadddr e)))
			    (ev (if (equal? (ev a env) '(&const #f)) c b) env)))

      ((eq? (car e) `&apply) (let ((f (cadr e))            ;; e == (&apply f args)
				   (args (caddr e)))
			       (let ((fv (ev f env))
				     (av (map (lambda (a) (ev a env)) args)))
				 (if (and (pair? fv) (eq? (car fv) '&const))
				     (delta fv av)
				     (apply-cl fv av)))))
      ((eq? (car e) `&let) (let ((f (caddr e))           
				   (args (cadr e)))
			       (let ((p (map (lambda (b) (car b) ) args))
				     (av (map (lambda (a) (ev (cadr a) env)) args)))
				 (ev f (extend* env p av)))))
      ((eq? (car e) `&let*) (let ((f (caddr e))           
				   (args (cadr e)))
				 (ev f (extend-env* env args))))
      )))

(define extend-env*
  (lambda (env m)
    (if (null? m) env (extend-env* (extend-env env (car m)) (cdr m)))
    ))
(define extend-env
  (lambda (env m)
    (let ((p (car m)) (av (ev (cadr m) env))) (extend env p av))))

(define evaluate
  (lambda (m)
    (unparse (ev (parse m) empty-env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M1 `((lambda (x) (+ x 1)) 5))
(define M35 `(list 1 2 3))
(define M30 `(let ((x 2) (y  (+ 2 1)) (z (+ 5 7))) (+ x y z)))
(define M33 `(let* ((x 2) (y  (+ x 1)) (z (+ y 7))) (+ x y z)))
(define M31 `(let ((x ((lambda (s) (+ 2 s)) 3))(y (+ 2 1))) (+ x y)))
(define M32 `((lambda (x)(let ((z 5)(y (+ 2 1))) (+ x y z))) 5))
(define M2 `((lambda (x) (+ x 2)) (if #t 3 2)))
(define M3 `(lambda (x y z) ((x y) z))) 
(define M4 '((lambda (y) (y 1 2 3)) (lambda (x z p) (+ x z p))))
(define M5 '((lambda (x) ( (lambda (x) (+ x 1)) 2 ) ) 1))
(define M6 `((lambda (x z) (x 1 (z 2 2))) + *))
(define M7 `((lambda () 1)))
(define M8 `((lambda (x) (+ x 0)) (+ 1 2)))
(define M9 '((lambda (x) x) (+ 1 2)))
(define M10 '((lambda (x) ((lambda (z) ((lambda (x) (z x)) 3)) (lambda (y) (+ x y)))) 1))
(define M11 '((lambda (x) 1) ((lambda (x) (x x)) (lambda (x) (x x)))))
(define M20 '( (lambda (x) ((lambda (z) ((lambda (x) (+ z x)) 2)) x)) 1))
(define M21 '((lambda (x y) (+ x y)) (+ 2 3) (+ 5 6)))
(define M40 '((lambda (y s) (y 1 2 3 s)) (lambda (x z p q ) (+ x z p q))2))



