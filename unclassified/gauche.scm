(define (reverse aList)
  (define (iter src dest)
    (if (null? src)
	dest
	(iter (cdr src) (cons (car src) dest))))
    (iter aList '()))

;; Ex 2.19.
(define (no-more? coin-values)
  (null? coin-values))
(define (except-first-denomination coin-values)

  (cdr coin-values))
(define (first-denomination coin-values)
  (car coin-values))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0)(no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

;; Ex 2.20.
(define (same-parity first . remain)
  (define (even? value)
    (= 0 (remainder value 2)))
  (let ((is-first-even? (even? first)))
       (define (same-parity? value)
	 (or (and is-first-even? (even? value))
	     (and (not is-first-even?) (not (even? value)))))
       (define (iter src dst)
	 (cond ((null? (cdr src)) (append dst src))
	       ((same-parity? (car src)) (iter (cdr src) (append dst (car src))))
	       (else
		(iter (cdr src) dst))))
       (iter remain (cons first '())))

;; Ex 2.25.
(define a (list 1 3 (list 5 7) 9)) ;; (1 3 (5 7) 9)
(define b (list (list 7))) ;; ((7))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))) ;; (1 (2 (3 (4 (5 (6 7))))))

(define (pick-7-from-a a)
  (car (cdr (car (cdr (cdr a))))))
(define (pick-7-from-b b)
  (car (car b)))
(define (pick-7-from-c c)
  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c)))))))))))))

;; Ex 2.26.
(define x (list 1 2 3))
(define y (list 4 5 6))

;; Ex 2.27
(define (reverse-1 items)
  (cons (cdr items) (car items)))
;; gosh> (reverse (list 1 2))
;; ((2) . 1)

(define (reverse-2 before-items)
  (define (iter items result)
    (if (null? (cdr items))
	(cons (car items) result)
	(iter (cdr items) (cons (car items) result))))
  (iter before-items '()))
;; gosh> (reverse-2 (list 1 2 3))
;; (3 2 1)

(define (reverse-3 before-items)
  (define (iter items result)
    (cond ((not (pair? items)) items)
	  ((null? (cdr items)) (cons (car items) result))
	  (else (iter (cdr items) (cons (car items) result)))))
  (iter before-items '()))
;; gosh> (reverse-3 1)
;; 1
;; gosh> (reverse-3 (list 1 2))
;; (2 1)
;; gosh> (reverse-3 (list (list 1 2)(list 3 4)))
;; ((3 4) (1 2))

(define (deep-reverse-1 items)
  (cons (reverse-3 (cdr items))
	(reverse-3 (car items))))
;; gosh> (deep-reverse (list (list 1 2)(list 3 4)))
;; (((3 4)) 2 1)

(define (deep-reverse-2 before-items)
  (define (iter items result)
    (cond ((not (pair? items)) items)
	  ((null? (cdr items)) (cons (car items) result))
	  (else (iter (iter (cdr items) '())
		      (cons (iter (car items) '()) result)))))
  (iter before-items '()))
;;gosh> (deep-reverse-2 (list (list 1 2)(list 3 4)))
;; ((3 4) (2 1))

(define (deep-reverse-3 before-items)
  (define (iter items result)
    (cond ((not (pair? items)) items)
	  ((null? (cdr items)) (cons (car items) result))
	  (else (iter (cons (iter (cdr items) '()) result)
		      (cons (iter (car items) '()) result)))))
  (iter before-items '()))
;; gosh> (deep-reverse-3 (list 1 2))
;; ((2) 1)

(define (deep-reverse-4 before-items)
  (define (iter items result)
    (cond ((not (pair? items)) items)
	  ((null? (cdr items)) (cons (car items) result))
	  (else (iter (iter (cdr items) '())
		      (cons (iter (car items) '()) result)))))
  (iter before-items '()))
