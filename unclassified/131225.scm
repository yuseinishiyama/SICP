;; Ex 3.28.

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; Ex 3.29

;; ド・モルガンの法則を用いる。
(define (or-gate i1 i2 output)
  (define (or-action-procedure)
    (let ((a (make-wire))
	  (b (make-wire))
	  (c (make-wire)))
      (inverter i1 a)
      (inverter i2 b)
      (and-gate a b c)
      (inverter c output)))
  (add-action! i1 (or-action-procedure))
  (add-action! i2 (or-action-procedure))
  'ok)
;; delayは
;; and-gate-delay + 2 * inverter-delay
;; (inverterは3つあるが、そのうち2つは並列で実行されるため)

;; Ex 3.30
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
	    ((eq? m 'set-signal!) set-my-signal!)
	    ((eq? m 'add-action!) accept-action-procedure!)
	    (else (error "Unknown operation --WIRE" m))))
    dispatch))

