(load "/Applications/Racket/scheme/ttt.scm")

;1.34
(define (square x) (* x x))

(define (f g)
  (g 2))

(f square)
;4

(f (lambda (z) (* z (+ z 1))))
;6

;if (f f) is evaluated it returns an error because f is not a recursive procedure

;1.37a
  ;(let ((a (/ n (+ d n)))))

(define (cont-frac n d k)
  (cond ((= k 0) 0)
        (else (/ n (+ d (cont-frac n d (- k 1)))))))

;1.38

(define (euler k)
  (cont-frac (lambda (x) 1)
             (lambda (x)
               (if (= (remainder x 3) 2) 
                           (/ (+ x 1) 1.5) 
                           1))
             k))

;1.41
(define (inc x) (+ x 1))

(define (double x)
  (lambda (x) (f (f x))))

;1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;1.43
(define (repeated f n)
  (cond ((equal? n 1) f)
        (repeated (lambda (x) (f (f x))) (- n 1))))

