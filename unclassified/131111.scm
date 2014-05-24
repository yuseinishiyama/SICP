;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(define (subsets s)
  (if (null? s)
      (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) x) rest)))))

(define (subsets s)
  (if (null? s)
      (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons s x)) rest)))))
