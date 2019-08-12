(define (same-parity x)
  (cond ((even? (first x))
         (cond ((= (count x) 0) '())
               ((even? (first x)) (cons (first x) (same-parity (bf x))))
               (else (odd? (first x)) (same-parity (bf x)))))
         (else (cond ((= (count x) 0) '())
                     ((odd? (first x)) (cons (first x) (same-parity (bf x))))
                     (else (even? (first x)) (same-parity (bf x)))))))

(define (max x)
  (cond ((= (count x) 1) x)
        ((> (first x) (car (bf x))) (max (cons (first x) (bf (bf x)))))
        (else (max (cons (car (bf x)) (bf (bf x)))))))

(define (maax a . b)
  (maxhelper (cons a b)))

(define (maxhelper l)
  (cond ((null? (cdr l)) (car l))
        ((< (car l) (cadr l)) (maxhelper (cdr l)))
        (else (maxhelper (cons (car l) (cddr l))))))

(define (max2 a . l)
  (cond ((null? l) a)
        ((< a (car l)) (apply max2 l))
        (else (apply max2 (cons a (cdr l))))))

(define (same-parity a . b)
  (cond ((null? b) a)
        ((even? a) (cons a (filter even? b)))
        (else (cons a (filter odd? b)))))

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (bf items)))))

(define (square-list2 items)
  (map square items))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (for-each proc item)
  (cond ((= (count item) 1) (proc (car item)))
        (else (proc (car item)) (for-each proc (cdr item)))))

(define (deep-reverse item)
  (cond ((null? item) '())
        ((list? (car item) (deep-reverse (append (cdr item) (deep-reverse (car item))))))
        (else (

