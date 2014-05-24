(cadr (list 1 2 3 4))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))

(define (square n) (* n n))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) n)
     ((divides? test-divisor n) test-divisor)
     (else
      (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (zero? (modulo b a)))
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))
	       
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove (lambda (a) (= a x)) s))))
	       s)))

(flatmap
 (lambda (i)
   (map (lambda (j) (list i j))
	(enumerate-interval 1 (- i 1))))
 (enumerate-interval 1 3))

(accumulate append
            '()
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 3)))

;; Ex 2.40.
(define (unique-pairs n)
	(flatmap
	 (lambda (i)
	   (map (lambda (j) (list i j))
		(enumerate-interval 1 (- i 1))))
	 (enumerate-interval 1 n)))

;; Ex 2.41.
(define (unique-triples n)
	(flatmap
	 (lambda (i)
	   (map (lambda (j) (append i (list j)))
		(enumerate-interval 1 (- (car (cdr i)) 1))))
	 (flatmap
	  (lambda (i)
	    (map (lambda (j) (list i j))
		 (enumerate-interval 1 (- i 1))))
	  (enumerate-interval 1 n))))

