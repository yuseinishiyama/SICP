;; Ex 2.44.
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))
	   
;; Ex 2.45.
(define (split op1 op2)
  (lambda (painter n)
    (if (= 0)
	painter
	(let ((smaller (split painter (- n 1))))
	  (op1 painter (op2 smaller smaller))))))

;; Ex 2.46.
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1)
		(xcor-vect vect2))
	     (+ (ycor-vect vect1)
		(ycor-vect vect2))))
(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1)
		(xcor-vect vect2))
	     (- (ycor-vect vect1)
		(ycor-vect vect2))))
(define (scale-vect s vect)
  (make-vect (* s (xcor-vect vect))
	     (* s (ycor-vect vect))))
