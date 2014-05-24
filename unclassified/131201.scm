;; assimilate
;; insatiable
;; decentralize
;; conglomerate
;; exploit
;; administrative
;; dismayed
;; hastily

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else (error "unknown expression typ -- DERIV" exp))))

;; Ex 2.73.
;; getによってderivというタグに対応した函数を呼び出す。


;; Ex 2.75.
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) 
	   (* mag (cos ang))
	  ((eq? op 'imag-part)
	   (* mag (sin ang)))
	  ((eq? op 'magnitude) mag)
	  ((eq? op 'angle) ang)
	  (else "Unknown op -- MAKE-FROM-MAG-ANG" op)))))

;; Ex 2.76.

;; 問題文で述べられているgenericの3つの方針について考えてみる。

;; "explicit dispatch"
;; これは条件文で型を判定する。
;; 名前のコンフリクト
	  
