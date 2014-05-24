;; rest on 基づいて

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) #t)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))
	 
;; Ex 2.63.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))
(define tree-a 
  (make-tree 7 
	     (make-tree 3 
			(make-tree 1 '() '())
			(make-tree 5 '() '()))
	     (make-tree 9
			'()
			(make-tree 11 '() '()))))
(define tree-b
  (make-tree 3
	     (make-tree 1
			'()
			'())
	     (make-tree 7
			(make-tree 5
				   '()
				   '())
			(make-tree 9
				   '()
				   (make-tree 11
					      '()
					      '())))))
(define tree-c
  (make-tree 5
	     (make-tree 3
			(make-tree 1
				   '()
				   '())
			'())
	     (make-tree 9
			(make-tree 7
				   '()
				   '())
			(make-tree 11
				   '()
				   '()))))

;; 結果はどれも同じようだ。
;; gosh> (tree->list-1 tree-a)
;; (1 3 5 7 9 11)
;; gosh> tree->list-2
;; gosh> (tree->list-2 tree-a)
;; (1 3 5 7 9 11)
;; gosh> (tree->list-1 tree-b)
;; (1 3 5 7 9 11)
;; gosh> (tree->list-2 tree-b)
;; (1 3 5 7 9 11)
;; gosh> (tree->list-1 tree-c)
;; (1 3 5 7 9 11)
;; gosh> (tree->list-2 tree-c)
;; (1 3 5 7 9 11)

;; 計算量の違いはappendを使っている分だけ、tree->list-1のほうが大きいらしい。

;; Ex 2.64.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; Ex 2.65.
;; list版
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
	((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (if (pair? set1) (car set1) set1)) 
	       (x2 (if (pair? set2) (car set2) set2)))
	   (cond ((= x1 x2)
		  (cons x1
			(union-set (cdr set1) (cdr set2))))
		 ((> x1 x2)
		  (cons x2
			(union-set set1 (cdr set2))))
		 ((< x1 x2)
		  (cons x1
			(union-set (cdr set1) set2))))))))

;; treeで同じことをしようとして辞めた。
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
	((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (if (pair? set1) (car set1) set1)) 
	       (x2 (if (pair? set2) (car set2) set2)))
	   (cond ((= x1 x2)
		  (make-tree x1
			     (union-set (cdr set1)
					(cdr set2))))
		 ((> x1 x2)
		  (make-tree x2
			     (union-set set1
					(cdr set2))))
		 ((< x1 x2)
		  (make-tree x1
			     (union-set 
		  (cons x1
			(union-set (cdr set1) set2))))))))

;; 答えを見たら単純だった。treeをlistに変換して処理してから、もう一度treeに戻している。
;; これらの処理のオーダーは全てnだから、nのオーダでできるということらしい。
;; treeのまま処理できないのだろうか。

(define (union-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (union-set list1 list2))))

(define (intersection-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (intersection-set list1 list2))))
