;;
;;  Ulrich Kremer
;;

;; computation does not terminate!
(define test-wrong
'(((lambda (f)
        ((lambda (x) (f (x x)))
         (lambda (x) (f (x x)))))
    (lambda (fac)
      (lambda (x)
        (if (equal? x 1) 1 (* x (fac (- x 1))))))) 3))


;; computes 3!
(define test-fac3
'(((lambda (f)
    ((lambda (x) (lambda (y) ((f (x x)) y)))
     (lambda (x) (lambda (y) ((f (x x)) y)))))
    (lambda (fac)
      (lambda (x)
        (if (equal? x 1) 1 (* x (fac (- x 1))))))) 3))

;; computes 5!
(define test-fac5
'(((lambda (f)
    ((lambda (x) (lambda (y) ((f (x x)) y)))
     (lambda (x) (lambda (y) ((f (x x)) y)))))
    (lambda (fac)
      (lambda (x)
        (if (equal? x 1) 1 (* x (fac (- x 1))))))) 5))
