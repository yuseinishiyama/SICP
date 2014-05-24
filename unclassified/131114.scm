(define (accumulate op ini

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Ex 2.38.

;; 右側から計算が行われる。fold-rightも同じ
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;; 左側から計算が行われる。
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;; 可換でない演算を行う場合は
;; 当然、fold-right/fold-leftで結果が異なってしまう。

;; Ex 2.39.

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
(define (reverse sequence)
  (fold-right (lambda (x y) (cons x y)) '() sequence))
