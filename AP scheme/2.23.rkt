(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (branch-weight branch)
  (let ((bs (branch-structure branch)))
    (if (list? bs)
        (total-weight bs)
        bs)))

(define (total-weight mobile)
  (+ (branch-structure (left-branch mobile)) (right-branch mobile)))

(define (square x ) (* x x))
;2.3
;(define (square-tree items)
;  (cond ((null? items) '())
;        ((eq? (count (car items)) 1) (cons (square (car items)) (square-tree (cdr items))))
;        (else (cons (square-tree (car items)) (square-tree (cdr items))))))
;2.31
(define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((eq? (count (car tree)) 1) (cons (proc (car tree)) (tree-map proc (cdr tree))))
        (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))
;2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                          (if (pair? x) (count-leaves x) 1)) t)))



