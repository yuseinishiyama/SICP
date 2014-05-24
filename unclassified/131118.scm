(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
	  (accumulate op initial (cdr seq)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; Ex2.48
(define (make-vect x y)
  (cons x y))
(define (make-segment vec1 vec2)
  (cons (vec1 vec2)))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
