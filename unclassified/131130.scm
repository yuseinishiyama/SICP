;; Ex 2.67.

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbol left) (symbol right))
	(+ (weight left) (weight right))))
			 
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbol tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (display current-branch)
    (newline)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (display next-branch)
	  (newline)
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; (A D A B B C A)


;; Ex 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;; ブランチに特定のシンボルが存在しているか。
(define (contain-symbol target tree)
  (if (not (memq target (symbol tree)))
      #f
      #t))

;; 特定のシンボルを符号化。
(define (encode-symbol target tree)
  (cond ((contain-symbol target (left-branch tree))
	 ;;　左側のブランチにシンボルが存在し、かつそのブランチがLeafであれば、0を返す。
	 (if (leaf? (left-branch tree))
	     (list 0)
	     ;; ブランチがさらに枝分かれしている場合は、再帰処理を行ったものの先頭に0を付与する。
	     (append (list 0) (encode-symbol target (left-branch tree)))))
	((contain-symbol target (right-branch tree)) 
	 (if (leaf? (right-branch tree))
	     (list 1)
	     (append (list 1) (encode-symbol target (right-branch tree)))))
	(else (error "BAD message -- " target))))

;; 正しく符号化できている。
;; gosh> (encode '(A D A B B C A) sample-tree)
;; (0 1 1 0 0 1 0 1 0 1 1 1 0)

;; 2.69.

;; わからなかったが、それは
;;  (This procedure is slightly tricky, but not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an ordered set representation.) 
;;の記述を見逃していたためである。
;; つまり想定する(sybol weight)のリストはwightをもとにソートしてあるのであり、下位2つを探す必要がなかった。

;; 解答は以下。(define (successive-merge leaf-set) 
(if (= (length leaf-set) 1) 
    (car leaf-set) 
    (let ((first (car leaf-set)) 
	  (second (cadr leaf-set)) 
	  (rest (cddr leaf-set))) 
      (successive-merge (adjoin-set (make-code-tree first second) 
				    rest))))) 
