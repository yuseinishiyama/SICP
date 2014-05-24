;; Ex 3.18

(define (contains-cycle? toplevel-list)
  (define (helper list)
    (cond ((not (pair? list))
	   (if (eq? list toplevel-list) #t
	       #f))
	  ((eq? (car list) toplevel-list) #t)
	  (else (helper (cdr list)))))
  (helper toplevel-list))

;; 循環リストの作成
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))

;; 間違い。解答を見る。
(define (cycle? x) 
  (define visited nil) 
  (define (iter x) 
    (set! visited (cons x visited)) 
    (cond ((null? (cdr x)) false) 
	  ((memq (cdr x) visited) true) 
	  (else (iter (cdr x))))) 
  (iter x)) 
;; 単純に一度チェックした箇所を局所変数に追加していく。

;; Ex 3.19.

;; フロイドの循環検出というアルゴルズムが有名なようだが、難解なため実装はせずに先に進む。

;; Ex 3.20.

;; キューのデータ構造を作成。
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) 
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))
;; さて、Bitdiddleと同じように実行すると、
;; gosh> (define q1 (make-queue))
;; q1
;; gosh> (insert-queue! q1 'a)
;; (#0=(a) . #0#)
;; gosh> (insert-queue! q1 'b)
;; ((a . #0=(b)) . #0#)
;; gosh> (delete-queue! q1)
;; (#0=(b) . #0#)
;; gosh> (delete-queue! q1)
;; (() b)
;; という風になるが、これはdelete-queue!でlistが殻になった時の処理、つまりrear-ptrがnullを指すようにする処理を入れていないからである。とはいえ、次にinset-queue!を実行した際にrear-ptr!は書き変わるので特に問題があるわけではないのだろう。
;; ではこのqueue構造を適切にprintできる函数を実装する。
;; listは結局のところlistの先頭のpairへの参照でしか無いので、front-ptrと同じ意味である。
(define (print-queue queue)
  (front-ptr queue))
;; 上記のq1をこの函数でprintすると
;; gosh> (print-queue q1)
;; ()

;; Ex 3.22.

;; こうして見てみると、
;; (set-front-ptr!　queue　item)というのはクラスメソッド、ユーティリティメソッドと等価の概念に見えるし、
;; (queue 'set-front-ptr! item)というのはインスタンスメソッドと等価の概念だ。
;; メッセージパッシングは一部の函数が変更されただけで、オブジェクト全体を評価しなおさないといけない。

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) 
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue" queue)
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set-front-ptr! new-pair)
	       (set-rear-ptr! new-pair)
	       (print-queue))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set-rear-ptr! new-pair)
	       (print-queue)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with an empty queue" queue))
	    (else
	     (set-front-ptr! (cdr front-ptr)))))
    (define (print-queue)
      front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
	    ((eq? m 'rear-ptr) rear-ptr)
	    ((eq? m 'set-front-ptr!) set-front-ptr!)
	    ((eq? m 'set-rear-ptr!) set-rear-ptr!)
	    ((eq? m 'empty-queue?) (empty-queue?))
	    ((eq? m 'front-queue) (front-queue))
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) (delete-queue!))
	    ((eq? m 'print-queue) (print-queue))
	    (else
	     (error "INVALID MESSAGE"))))
    dispatch))

;; 動作確認。外部に公開する必要のない函数も公開してしまっているかもしれない。
(define q1 (make-queue))
((q1 'insert-queue!) 'a)
;; gosh> (a)
((q1 'insert-queue!) 'b)
;; gosh> (a b)
(q1 'delete-queue!)
;; gosh> (b)
(q1 'delete-queue!)
;; gosh> (b)
((q1 'insert-queue!) 'c)
;; gosh> (b)

;; Ex 3.23.

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque) 
  (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)))
(define (front-insert-deque! deque item)
   (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! new-pair (front-ptr deque))
	   (set-front-ptr! deque new-pair)
	   deque))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-cdr! (rear-ptr deque) new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	 (set-front-ptr! deque (cdr (front-ptr deque)))
	 deque)))  
;; 最後から一つ前のペアを参照していないといけない！！
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "DELETE! called with an empty deque" deque))
	(else
	 (set-reae-ptr! deque (cdr (front-ptr deque)))
	 deque)))  
(define (print-deque deque)
  (front-ptr deque))
    
(define dq (make-deque))
(front-insert-deque! dq 'a)
(front-insert-deque! dq 'b)
