;; Ex 3.23.

;; 末尾のセルを1のオーダーで削除するために、双方向リストが必要なことは分かった。ただし、実装に手間がかかりすぎるため、飛ばす。

;; deque-item
(define (deque-item-value deque-item)
  (car deque-item))
(define (deque-item-prev deque-item)
  (cadr deque-item))
(define (deque-item-next deque-item)
  (caddr deque-item))

(define (make-deque)
  (list '() '()))
(define (empty-deque? deque)
  (null? (front-deque deque)))
(define (front-deque deque)
  (car deque))
(define (rear-deque deque)
  (cdr deque))
(define (print-deque deque)
  (front-deque deque))
(define (front-insert-deque! deque item)
  (let ((next (front-deque deque))
	(deque-item (list item '() next)
    (if (empty-deque? deque)
      ;;
      (set! front-deque (list item '() next)))))
  
;; Ex 3.24. - 3.26.
;; 同じく記述量が多そうなため飛ばす。

;; Ex 3.27
;; 問題文にあるmemorizeのコードが理解できない。
