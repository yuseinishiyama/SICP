;;Ex 2.53.

(list 'a 'b 'c)

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue shocks)))

(memq 'red '(red shoes blue shocks))

;; これは(shoes blue shocks)となる。つまり、その文字列が存在する以降のリストを返却する。
(memq 'shoes '(red shoes blue shocks))

;; 2.3.2
;; (symbol?)は文字列かどうか。
;; (pair?)は3つ以上でも可。

;; Ex 2.56.
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;;(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
;;(define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (pow x y)
  (if (= y 0)
      1
      (if (= (modulo y 2) 0)
	  (* (pow x (/ y 2))(pow x (/ y 2)))
	  (* x (pow x (- y 1))))))
(define (exponentiation? e)
  (and (pair? e) 
       (eq? (car e) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
	((=number? e2 1) e1)
	((and (number? e1) (number? e2)) (pow e1 e2))
	(else
	 (list '** e1 e2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-exponentiation (base exp) (- (exponent exp) 1))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

