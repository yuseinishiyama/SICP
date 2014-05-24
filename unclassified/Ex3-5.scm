(use slib)
(require 'trace)

;; Streamの実装
(define (memo-proc proc)
  (let ((already-run? #f)
	(result #f))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? #t)
		 result)
	  result))))

;; gaucheではテキストのdelayが遅延実行されない。
;; (define (delay exp)
;;  (memo-proc (lambda() exp)))
(define-macro (delay x)
              `(memo-proc (lambda () ,x)))
;; (define (cons-stream a b)
;;  (cons a (delay b)))
(define-macro (cons-stream a b)
              `(cons ,a (delay ,b)))
(define (force delayed-object)
  (delayed-object))
(define the-empty-stream 'the-empty-stream)
(define (stream-null? stream)
  (eq? stream the-empty-stream))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; utility
(define (stream-map proc s)
  the-empty-stream
  (cons-stream (proc (stream-car s))
	       (stream-map proc (stream-cdr s))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
	 the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else 
	 (stream-filter pred 
			(stream-cdr stream)))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))))
(define (stream-head stream num)
  (define (iter stream count)
    (if (> count num)
	'done
	(begin (display (stream-ref stream count))
	       (newline)
	       (iter stream (+ count 1)))))
  (iter stream 0))

;; test
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(stream-ref fibs 1)

;; Ex3.50.
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

;; Defining streams implicitly

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(trace integers)
(stream-head integers 10)
(trace stream-head)


;; 遅延評価によって無限長や無限循環を表現することが可能になる。

;; Ex3.53.
;; 1 2 4 8 16 32

;; Ex3.54.
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (div-streams s1 s2)
  (stream-map / s1 s2))
(define factorials 
  (cons-stream 1
	       (mul-streams factorials 
			    (add-streams ones
					 integers))))

;; Ex3.55.
(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams
		(stream-cdr s)
		(partial-sums s))))

;; Ex3.56.
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))
;; 5の倍数のlist、3の倍数のlist、2の倍数のlistをmergeしたもの。
(define S (cons-stream 1 (merge (scale-stream S 2) 
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))
;; Ex3.57.
;; すでに実行された加算を毎回実行する。

;; Ex3.58.
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
(define test1 (expand 1 7 10))
(stream-head test1 10)
;; 割り算の手計算のような結果になる。
;; gosh>
;; 1
;; 4
;; 2
;; 8
;; 5
;; 7
;; 1
;; 4
;; 2
;; 8
;; 5
;; done

;; Ex3.59.
(define (integrate-series stream)
  (div-streams stream
	       integers))
(stream-head (integrate-series integers) 10)
(define exp-series
  (cons-stream 1
	       (integrate-series exp-series)))
(stream-head exp-series 10)

(define cosine-series
  (cons-stream 1
	       (integrate-series sine-series)))
(define sine-series
  (cons-stream 0
	       (integrate-series cosine-series)))

;; Ex3.60.
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
		  (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2)
					  (stream-car s1))
			    (mul-series (stream-cdr s1) s2))))
(define a (add-streams (mul-series cosine-series
				   cosine-series)
		       (mul-series sine-series
				   sine-series)))
(stream-head a 10)

;; Ex3.61 62
;; 飛ばす

;; Ex3.63.
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 #?=(stream-map (lambda (guess)
				  (sqrt-improve guess x))
				guesses)))
  guesses)
(define (sqrt-stream-2 x)
  (cons-stream 1.0
	       #?=(stream-map (lambda (guess)
				(sqrt-improve guess x))
			      (sqrt-stream x))))
(stream-head (sqrt-stream 2) 10)
(stream-head (sqrt-stream-2 2) 20)
;; 下のバージョンではメモライズされないようだが、あまり良くわからない。

;; Ex.3.64
(define (stream-limit stream tolerance)
  (let ((s-car (stream-car stream))
	(s-cadr (stream-car (stream-cdr stream))))
    (if (> tolerance (abs (- s-car s-cadr)))
	s-car
	(stream-limit (stream-cdr stream) tolerance))))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
      
;; Ex.3.65
;; 収束の加速の概念がよくわからないので飛ばす。
;; http://www.sato.kuis.kyoto-u.ac.jp/~igarashi/class/pl/slides7.pdf
;; のスライドが良さそう。

;; Ex.3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
(stream-head (pairs integers integers) 100)
;; interleaveが再帰的に行われる。
;; 規則性は分からない。
;; gosh> CALL stream-head ((1 ...) . #[proc]) 100
;; (1 1)
;; (1 2)
;; (2 2)
;; (1 3)
;; (2 3)
;; (1 4)
;; (3 3)
;; (1 5)
;; (2 4)
;; (1 6)
;; (3 4)
;; (1 7)
;; (2 5)
;; (1 8)
;; (4 4)
;; (1 9)
;; (2 6)
;; (1 10)

;; Ex.3.67.
(define (all-pairs s t)
  (cons-stream 
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t)))
    (stream-map (lambda (x) (list x (stream-car t)))
		(stream-cdr s)))))
(stream-head (all-pairs integers integers) 10)

;; Ex.3.68.
;; (define (pairs s t)
;;   (interleave
;;    (stream-map (lambda (x) (list (stream-car s) x))
;;                t)
;;    (pairs (stream-cdr s) (stream-cdr t))))
;; (stream-head (pairs integers integers) 10)

;; (pairs (stream-cdr s) (stream-cdr t))に遅延評価が働かないため無限ループに陥る。

;; Ex.3.69.
(define (triples s1 s2 s3)
  (cons-stream (list (stream-car s1)
		     (stream-car s2)
		     (stream-car s3))
	       (interleave 
		(stream-map (lambda(x) (append (list (stream-car s1)) x))
			    (stream-cdr (pairs s2 s3)))
		(triples (stream-cdr s1)
			 (stream-cdr s2)
			 (stream-cdr s3)))))
(stream-head (triples integers integers integers) 10)
;; 解答を参考にしたが、pairsを入れ子にした形で実装できそうな気もする。

(define (phythagorean-numbers)
  (define (square x) (* x x))
  (define numbers (triples integers integers integers))
  (stream-filter (lambda (x)
		   (= (square (caddr x))
		      (+ (square (car x)) (square (cadr x)))))
		   numbers))
(stream-head (phythagorean-numbers) 5)

;; Ex.3.70-72
;; 飛ばす
		  
;; Ex.3.73
;; 回路がわからないので飛ばす

;; Ex.3.74
;; 後続のsense-dataと比較し続ける。
;; (define zero-crossing
;;   (stream-map sign-change-detector 
;; 	      sense-data 
;; 	      (stream-car (stream-cdr sense-data))))
;; これだと前にずらしてしまう。後ろにずらすのが正しい。

;; Ex.3.75 - 76
;; 簡単そうなので飛ばす。

(define (integral integrand initial-value dt)
  (define int 
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)
;; S3までを求める様子
;; C r1dt r2dt r3dt ...
;;   C    r1dt r2dt ...
;;        C    r1dt ...
;; +)          C    ...

;; 3.5.5 Modularity of Functional Programs and Modularity of Objects

(define random-numbers
  (cons-stream random-init
	       (stream-map rand-update
			   random-numbers)))
(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd (r1 r2)) 1)) ;;連続する2数同士が素数かどうか
			random-numbers))
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))
(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
	      (monte-carlo cesaro-stream 0 0)))

;; Ex 3.81. - Ex.3.82
;; 飛ばす

;; A functional-programming view of time.
;; computational objectsとは状態を持った関数のことである。評価することで状態を変更することができる。

;; 時間とComputatioanl Objectsの比較。
;; 状態を持つオブジェクトを作成するか、初期値と変化量と時間から状態を算出するか。
;; 後者である関数型言語は変更されないwell-defined mathematical functionsであり、並列処理に向いている。
