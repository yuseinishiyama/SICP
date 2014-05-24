;; thicket

;; 引数なしの函数を(func)として定義するか、funcとして定義するか悩む。
;; ただ、下記の例からも明らかなように、函数のオブジェクトと、函数の実行を明確に区別する必要がある。
;; 上はsome-methodを実行する函数testであり、下はsome-methodを返す函数testだ。

(define (test)
  (define some-method "Test")
  some-method)
(define (test)
  (define (some-method) "Test")
  some-method)

;; 下記のものをsome-methodを実行するものに書き換えるとこうだ。
(define (test)
  (define (some-method) "Test")
  (some-method))

;; 函数をオブジェクトとして扱う以上、引数なしの函数をカッコなしで定義するのはあまりよい方法でないと感じる。

;; Ex 3.5.

;; モンテカルロ法を利用し、単位円の面積を算出する。

;; ランダム数生成のため
(use srfi-27)

(define (random num)
;;  (random-integer (+ num 1)))
  (* (random-real) num))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (rectangle-space)
    (* (- x2 x1)
       (- y2 y1)))
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (rectangle-space) 1.0)
     (monte-carlo trials test)))

(define (estimate-pi trials)
  (define (unit-circle)
    (lambda (x y) (>= 1 (+ (* x x)
			   (* y y)))))
  (estimate-integral (unit-circle)
		     -1 1 -1 1
		     trials))

;; gosh> (estimate-pi 10)
;; 3.6
;; gosh> (estimate-pi 100)
;; 3.2
;; gosh> (estimate-pi 1000)
;; 3.196
;; gosh> (estimate-pi 10000)
;; 3.1216
;; gosh> (estimate-pi 100000)
;; 3.13956
;; gosh> (estimate-pi 1000000)
;; 3.142192
;; gosh> (estimate-pi 1000000)
;; 3.143176

;; Ex 3.6.

;; 線形合同法を用いた簡易的な乱数。
;; http://www001.upp.so-net.ne.jp/isaku/rand.html
(define (make-random initial-value)
  (let ((x initial-value))
    (define (rand-update x)
      (modulo (+ (* x 1103515245)
		 12345)
	      2147483647))
    (define (reset new-value)
      (set! x new-value))
    (define (generate)
      (begin (set! x (rand-update x))
	     x))
    (define (dispatch message)
      (cond ((eq? message 'generate)
	     (generate))
	    ((eq? message 'reset)
	     reset)
	    (else "Invalid Operation")))
    dispatch))

;; gosh> (define random-generator (make-random 10))
;; random-generator
;; gosh> (random-generator 'generate)
;; 297746560
;; gosh> (random-generator 'generate)
;; 1077167153

;; 同じシードで初期化。
;; gosh> ((random-generator 'reset) 10)
;; 10
;; gosh> (random-generator 'generate)
;; 297746560

;; シードを変更。
;; gosh> ((random-generator 'reset) 11)
;; 11
;; gosh> (random-generator 'generate)
;; 1401261805
