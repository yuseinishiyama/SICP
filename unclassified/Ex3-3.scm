vocavular:
drawback
propagate
stipulate
suffice
equipotent
informant
impoverish

強力な言語に必要な3つのメカニズム
1.primitive expression
2.means of combination
3.means of abstraction

;; Ex 3.31
(define (inverte input output)
  (define (inverter-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and a b)
  (cond ((= a 0)
	 (cond ((= b 0) 0)
	       ((= b 1) 0)
	       (else (error "Invalid signal" b))))
	((= a 1)
	 (cond ((= b 0) 0)
	       ((= b 1) 1)
	       (else (error "Invalid signal" b))))
	(else (error "Invalid signal" a))))
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc));; 登録する際に一度実行するのはなぜかという問題。
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operation -- WIRE" m))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
	((car procedures))
	(call-each (cdr procedures)))))
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedures)
  ((wire 'add-action!) action-procedures))

wireの値が変わらない限り実行されないため、初期化時に実行しておく必要がある。


;; Ex 3.32
実行するたびにlistの長さ分だけ探索しないといけないため、list構造は適さないと言える。
(解答を見る限り実行順序が違うというのが答だった。)

;; 注29 else部のないif(one-armed if)
(define (one-armed-if a)
  (if (null? a)
      (display a)))
;; 実行結果。undefinedになっている。
;; gosh> (one-armed-if 1)
;; #<undef>

;; 3.3.5
制約プログラミングについて。



