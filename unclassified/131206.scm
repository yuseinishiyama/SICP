;; Ex 3.1.

(define (make-accumulator sum)
  (lambda (input)
    (begin (set! sum (+ sum input))
	   sum)))

;; Ex 3.2.

;; 以下の場合だと、なんとmake-monitored定義時にmfが1と評価されてしまう。
;; defineの定義時の挙動が複雑な模様...

;; (define (make-monitored f)
;;   (let ((count 0))
;;     (define (mf arg)
;;       (cond ((eq? arg 'how-many-calls?) count)
;; 	    (else (begin (set! count (+ count 1))
;; 			 (f args)))))))

;; lambdaだと定義時には評価されない。

(define (make-monitored f)
  (let ((count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) count)
	    ((eq? arg 'reset
	    (else (begin (set! count (+ count 1))
			 (f arg)))))))

;; 以下の2つを定義してみるとわかるが、静的なコードは定義時に実行されてしまうようだ。

(define test
  (display "a"))

(define (test2 a)
  (display a))

;; letの挙動
(define test3
  (let ((a 3))

;; Ex 3.3
(define (make-account balance password)
  (let ((p password))
    (define (withdraw amount)
      (if (>= balance amount)
	;  (begin (set! balance (- balance amount))
	;	 balance)
	  "Insufficient funds"))
    (define (deposit amount)
;      (set! balance (+ balance amount))
 ;     balance)
    (define (correct-pass? pass)
      (if (eq? password password)
	  #t
	  #f))
    (define (dispatch m pass)
      (cond ((correct-pass? pass) (display "correct"))
	    (else "incorrect...")))))
    
    
