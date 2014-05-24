;; Ex 2.33.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))
(define (map p sequences)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequences))
(define (append seq1 seq2)
  (accumulate cons (accumulate cons '() seq2) seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Ex 2.34
(define (pow x n)
  (define (iter result k)
    (if (= n k)
	result
	(iter (* x result) (+ 1 k))))
  (iter 1 0))
    
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff
						   (* x higher-terms)))
	      0
	      coefficient-sequence))
	       
;; Ex 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (node) 
			 (if (pair? node)
			     (count-leaves node)
			     1))
		       t)))
