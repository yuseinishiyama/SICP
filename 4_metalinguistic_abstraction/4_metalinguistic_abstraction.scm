(define apply-in-underlying-scheme apply)

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

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
;;	((and? exp) (eval-and exp env)) ;; Ex 4.4
	((and? exp) (eval (and->if exp) env))
;;	((or? exp) (eval-or (or-predicates exp) env)) ;; Ex 4.4
	((or? exp) (eval (or->if exp) env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
;;	((cond? exp) (eval (cond->if exp) env)) ;; Ex 4.5
        ((cond? exp) (eval (cond->if exp env) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		   (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

; 複数の被演算子を評価する。
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; definition
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
'ok)
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp) ; (define <var> <value>)
      (caadr exp))) ; (define (<var> <parameter> ...) <body>)
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp) ; (define <var> <value>)
      ; (define (<var> <parameter> ...) <body>)の糖衣構文の場合は、
      ; ラムダ式を使用した文に書き直す。
      ; (define <var> (lambda (<paramter> ...) <body>))
      (make-lambda (cdadr exp)
		   (cddr exp))))

;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false)) ; else句の実装がなければfalseを返す。
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;;;;;;;;;;;;;;;;;;;; Ex 4.4 ;;;;;;;;;;;;;;;;;;;;
;; and
(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (rest-exps exp))
(define (eval-and exp env)
  (define (eval-iter predicates result env)
    (cond ((null? predicates) #t)
	  ((not (pair? predicates)) result)
	  ((not (eval (car predicates) env)) #f)
	  (else (eval-iter (cdr predicates) result env))))
  (eval-iter (and-predicates exp) '() env))
;; or
(define (or? exp) (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))
(define (eval-or exps env)
  (if (last-exp? exps) 
      (eval (first-exp exps) env)
      (if (eval (first-exp exps) env)
	  #t
	  (eval-or (rest-exps exps) env))))

;; and (derivered)
(define (and->if exp) (expand-and (rest-exps exp)))
(define (expand-and predicates)
  (if (null? predicates)
      'true
      (make-if (first-exp predicates)
	       (expand-and (rest-exps predicates))
	       'false)))
;; or (derivered)
(define (or->if exp) (expand-if (rest-exps exp)))
(define (expand-if predicates)
  (if (null? predicates)
      'false
      (make-if (first-exp predicates)
	       'true
	       (expand-if (rest-exps predicates)))))


;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
;;;;;;;;;;;;;;;;;;;; Ex 4.5 ;;;;;;;;;;;;;;;;;;;;
;; (define (cond->if exp)
;;   (expand-clauses (cond-clauses exp)))
;; (define (expand-clauses clauses)
;;   (if (null? clauses)
;;       'false
;;       (let ((first (car clauses))
;; 	    (rest (cdr clauses)))
;; 	(if (cond-else-clause? first)
;; 	    (if (null? rest)
;; 		(sequence->exp (cond-actions first))
;; 		(error "ELSE clauses isn't last -- COND-IF"
;; 		       clauses))
;; 	    (make-if (cond-predicate first)
;; 		     (sequence->exp (cond-actions first))
;; 		     (expand-clauses rest))))))
(define (cond->if exp env)
  (expand-clauses (cond-clauses exp) env))
(define (additional-syntax-proc clause)
  (caddr clause))
(define (additional-syntax-clause? clause)
  (if (pair? (cdr clause))
      (eq? (cadr clause) '=>)
      #f))
(define (expand-clauses clauses env)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clauses isn't las -- COND-IF"
                          clauses)))
              ((additional-syntax-clause? first)
               (make-if (cond-predicate first)
                        (eval (list (additional-syntax-proc first) (cond-predicate first)) env)
                        (expand-clauses rest env)))
              (else 
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest env)))))))
;; 実行結果
;; gosh> (cond ('(a b c) => car))
;; a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;predicate	    
(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

;; procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; environment
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; frame
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! (vals val)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define (setup-environment)
  (let ((initial-env 
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

;; primitives
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	;; more primitives
))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitiv-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define the-global-environment (setup-environment))

;; 出力結果のBool値を調整する。 
(define (true-false-adjustment objects)
  (define (convert object)
    (cond ((eq? object '#t) 'true)
          ((eq? object '#f) 'false)
          (else object)))
  (if (not (pair? objects))
      (convert objects)
      (let ((first (car objects))
            (rest (cdr objects)))
               (cons (true-false-adjustment (convert first)) (true-false-adjustment (convert rest))))))

;;(true-false-adjustment '(a b c))
;;(true-false-adjustment '(a (#t #f) c))
;;(true-false-adjustment '#f)

;; REPL
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))
(define (announce-output string)
  (newline)
  (display string)
  (newline))
(define (user-print object)
  (let ((display-string (if (compound-procedure? object)
                            (list 'compound-procedure
                                  (procedure-parameters object)
                                  (procedure-body object)
                                  '<procedure-env>)
                            object)))
    (display (true-false-adjustment display-string))))

