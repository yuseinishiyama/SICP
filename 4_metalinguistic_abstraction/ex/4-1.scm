; Ex.4.1

 (if (null? operands)
      #t
      #f))
(define (first-operand operands) (car operands))
(define (rest-operands operands) (cdr operands))

; 左から右へ評価する。
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (eval (first-operand exps) env)))
           (cons first-eval
                 (list-of-values (rest-operands exps) env)))))

;; 右から左へ評価する。
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (list-of-values (rest-operands exps) env)))
           (cons (eval (first-operand exps) env)
                 first-eval))))
