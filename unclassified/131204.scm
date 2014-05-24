(define (add-poly p1 p 2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (add-terms (term-list p1)
			    (term-list p2)))
      (error "Ploys not in same var -- ADD-POLY"
	     (list p1 p2))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (mul-terms (term-list p1)
			    (term-list p2)))
      (error "Ploys not in same var -- MUL-POLY"
	     (list p1 p2))))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))
