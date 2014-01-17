(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operation exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitiv-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error "Unknown procedure type -- APPLY" procedure))))

; 複数の被演算子を評価する。
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exp env)
  (cond ((last-exp? exp) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
'ok)

; Ex.4.1
(define (display-a)
  (display "a")
  (newline)
  'a)
(define (display-b)
  (display "b")
  (newline)
  'b)
(define (no-operands? operands)
  (if (null? operands)
      #t
      #f))
(define (first-operand operands) (car operands))
(define (rest-operands operands) (cdr operands))

; まず現在の処理系での評価順序を確認する。
(cons (display-a) (display-b))
; a
; b
; (a . b)

; 左から右へ評価していることが分かったので、右から左への評価を行うよう実装する。

; 1.こうだと今度はletの評価順に依存してしまう。
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (eval (rest-operands exps) env))
	    (left (eval (first-operand exps) env)))
	(cons left right))))
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
	    (right (eval (rest-operands exps) env)))
	(cons left right))))

 
; 2.そこでletを入れ子にする、もしくはlet*を使うことで実行順序を決定する。
 (define (list-of-values-lr exps env) 
   (if (no-operand? exps) 
       '() 
       (let* ((left (eval (first-operand exps) env)) 
                 (right (eval (rest-operands exps) env))) 
         (cons left right)))) 
; 3.そもそも先に評価したい式だけをlet内で評価すれば良い。
