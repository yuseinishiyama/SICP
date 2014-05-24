;; Ex 3.1.

(define (make-accumulator sum)
  (lambda (input)
    (begin (set! sum (+ sum input))
	   sum)))

;; Ex 3.2.

;; �ʲ��ξ����ȡ��ʤ��make-monitored�������mf��1��ɾ������Ƥ��ޤ���
;; define��������ε�ư��ʣ��������...

;; (define (make-monitored f)
;;   (let ((count 0))
;;     (define (mf arg)
;;       (cond ((eq? arg 'how-many-calls?) count)
;; 	    (else (begin (set! count (+ count 1))
;; 			 (f args)))))))

;; lambda����������ˤ�ɾ������ʤ���

(define (make-monitored f)
  (let ((count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) count)
	    ((eq? arg 'reset
	    (else (begin (set! count (+ count 1))
			 (f arg)))))))

;; �ʲ���2�Ĥ�������Ƥߤ�Ȥ狼�뤬����Ū�ʥ����ɤ�������˼¹Ԥ���Ƥ��ޤ��褦����

(define test
  (display "a"))

(define (test2 a)
  (display a))

;; let�ε�ư
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
    
    
