;; quotation
;; wreak
;; havoc
;; coherent
;; violate
;; skirts
;; stipulate
;; considerable
;; moderate

;; superlative
;; stupendous

;; x��set��¸�ߤ��뤫��
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

;; x��set��¸�ߤ��Ƥ��ʤ�����ɲä��롣
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; set1��set2��ξ���˴ޤޤ�Ƥ����Τ�set
(define (intersection-set set1 set2)
  (cond ((or (null? set1)(null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	 (else
	  (intersection-set (cdr set1) set2))))

;; Ex 2.59.
;; ��ʣ���������set
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
	((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else 
	 (cons (car set1)
	       (union-set (cdr set1) set2)))))
	
;; gosh> (union-set (list 1 2 3) (list 3 5 6))
;; (1 2 3 5 6)
;; gosh> (union-set (list 1 2) (list 1))
;; (2 1)
;; gosh> (union-set (list 1 2 3) '())
;; (1 2 3)
;; gosh> (union-set '() (list 1 2 3))
;; (1 2 3)

;; Ex 2.60.
;; �Ĥޤꡢ�����������Ȥ������äƤ��ޤ����Ȥ��򤱤�����
;; gosh> (intersection-set (list 1 1 1) (list 1 1 1))
;; (1 1 1)

(define (no-duplicate-set set)
  (cond ((null? set) '())
	((element-of-set? (car set) (cdr set))
	 (no-duplicate-set (cdr set)))
	(else (cons (car set)
		    (no-duplicate-set (cdr set))))))

;; gosh> (no-duplicate-set (list 1 1 1))
;; (1)

;; �����n^2�Υ��������Ǥ��롣

;; ������ä�����ʣ������С�������������餷����
;; ���⤽��set����ʣ���Ƥ������ͤ�������Ǥ�̵���褦����

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

;; Ex 2.61.
(define (adjoin-set x set)
  (cond ((< x (car set)) set)
	((element-of-set? x set) set)
	(else (cons x set))))
;; ������ä������⤽��element-of-set?�Ȥ�����ǤϤʤ���

;; Ex 2.62.
(define (union-set set1 set2)
  (let ((x1 (if (pair? set1) (car set1)))
	(x2 (if (pair? set2) (car set2))))
    (cond ((and (null? set1) (null? set2)) '())
	  ((null? set1) set2)
	  ((null? set2) set1)
	  ((> x1 x2)
	   (cons x2
		 (union-set set1 (cdr set2))))
	  ((< x1 x2)
	   (cons x1
		 (union-set (cdr set1) set2)))
	  ((= x1 x2)
	   (cons x1
		 (union-set (cdr set1) (cdr set2)))))))
;; ����Ϥܴۤ���


(use slib)
(require 'trace)
(trace union-set)
