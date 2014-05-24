;; Ex 3.12.

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
;; (cdr x) => (b)
;; xは変更されていない。

(define w (append! x y))
;; (cdr x) => (b c d)
;; x自体が変更されている。

;; Ex 3.13.

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))

;; Ex 3.14.

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (display temp)
	  (newline)
	  (loop temp x))))
  (loop x '()))
(define v (list 'a 'b 'c 'd))
;; gosh> (mystery v)
;; (d c b a)
;; 逆順となる。
