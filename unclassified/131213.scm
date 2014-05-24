;; forgo
;; reasoning

(define (make-decrement balance)
  (lambda (amount)
    (- balance amount)))
