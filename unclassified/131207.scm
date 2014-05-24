;; thicket

;; �����ʤ���ȡ����(func)�Ȥ���������뤫��func�Ȥ���������뤫Ǻ�ࡣ
;; �������������㤫������餫�ʤ褦�ˡ�ȡ���Υ��֥������Ȥȡ�ȡ���μ¹Ԥ����Τ˶��̤���ɬ�פ����롣
;; ���some-method��¹Ԥ���ȡ��test�Ǥ��ꡢ����some-method���֤�ȡ��test����

(define (test)
  (define some-method "Test")
  some-method)
(define (test)
  (define (some-method) "Test")
  some-method)

;; �����Τ�Τ�some-method��¹Ԥ����Τ˽񤭴�����Ȥ�������
(define (test)
  (define (some-method) "Test")
  (some-method))

;; ȡ���򥪥֥������ȤȤ��ư����ʾ塢�����ʤ���ȡ���򥫥å��ʤ����������ΤϤ��ޤ�褤��ˡ�Ǥʤ��ȴ����롣

;; Ex 3.5.

;; ���ƥ����ˡ�����Ѥ���ñ�̱ߤ����Ѥ򻻽Ф��롣

;; ������������Τ���
(use srfi-27)

(define (random num)
;;  (random-integer (+ num 1)))
  (* (random-real) num))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (rectangle-space)
    (* (- x2 x1)
       (- y2 y1)))
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (rectangle-space) 1.0)
     (monte-carlo trials test)))

(define (estimate-pi trials)
  (define (unit-circle)
    (lambda (x y) (>= 1 (+ (* x x)
			   (* y y)))))
  (estimate-integral (unit-circle)
		     -1 1 -1 1
		     trials))

;; gosh> (estimate-pi 10)
;; 3.6
;; gosh> (estimate-pi 100)
;; 3.2
;; gosh> (estimate-pi 1000)
;; 3.196
;; gosh> (estimate-pi 10000)
;; 3.1216
;; gosh> (estimate-pi 100000)
;; 3.13956
;; gosh> (estimate-pi 1000000)
;; 3.142192
;; gosh> (estimate-pi 1000000)
;; 3.143176

;; Ex 3.6.

;; ������Ʊˡ���Ѥ����ʰ�Ū�������
;; http://www001.upp.so-net.ne.jp/isaku/rand.html
(define (make-random initial-value)
  (let ((x initial-value))
    (define (rand-update x)
      (modulo (+ (* x 1103515245)
		 12345)
	      2147483647))
    (define (reset new-value)
      (set! x new-value))
    (define (generate)
      (begin (set! x (rand-update x))
	     x))
    (define (dispatch message)
      (cond ((eq? message 'generate)
	     (generate))
	    ((eq? message 'reset)
	     reset)
	    (else "Invalid Operation")))
    dispatch))

;; gosh> (define random-generator (make-random 10))
;; random-generator
;; gosh> (random-generator 'generate)
;; 297746560
;; gosh> (random-generator 'generate)
;; 1077167153

;; Ʊ�������ɤǽ������
;; gosh> ((random-generator 'reset) 10)
;; 10
;; gosh> (random-generator 'generate)
;; 297746560

;; �����ɤ��ѹ���
;; gosh> ((random-generator 'reset) 11)
;; 11
;; gosh> (random-generator 'generate)
;; 1401261805
