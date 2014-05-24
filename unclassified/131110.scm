;; Ex 2.30.

;; scale-treeで実装されているパターンをそのまま適応してみる。
;; ツリーの順序がそのままであれば (cons (car ...) (cdr ...))でleafまで辿り着くことができる。
;; 後はnilの判定と、leafの判定を正しく行えば、全てのleafに対し処理を行うことができる。
 
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

;; こちらもscale-treeと全く同じ形。なぜ、子要素までなめることができるのかよくわからない。
;; mapがそういう風にできているのだろう。
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

;; 同じ方法でdeep-reverseを実装してみる。
;; nilの場所のせいで、ツリーの形が崩れてしまう。
;; gosh> (deep-reverse (list 1 2))
;; ((() . 2) . 1)
;; やはり、ツリーの順番を操作するのはコストがかかるようだ。
(define (deep-reverse tree)
  (cond ((null? tree) '())
	((not (pair? tree)) tree)
	(else (cons (deep-reverse (cdr tree))
		    (deep-reverse (car tree))))))

