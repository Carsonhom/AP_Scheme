 
;By Carson Hom and Sebastion Detering

;1.3
(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next (term a)) (+ result a))))
    (iter a 0))

;1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next (term a)) (* result a))))
  (iter a 1))

(define (factorial x)
  (define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next (term a)) (* result a))))
  (iter a 1))
  (product term 1 next x))

(define (pi-term n)
  (cond ((odd? n) (/ (+ n 1) (+ n 2)))
        (else (/ (+ n 2) (+ n 1)))))

(define (pi/4 terms)
  (product pi-term 1 1+ terms))

;1.32


(define (accumulate combiner null-value term a next b)
  (define (iteraterater combiner a result)
    (if (> a b)
        result
        (iter (next (term a)) (combiner result a))))
  (iteraterater combiner a (cond ((equals? combiner +) 0)
                                 (else 1))))

(define (accumulate combiner null-value term a next b)
  (define
                               

;1.33
(define (gcd a b)
  (let ((r (remainder a b)))
    (cond ((=  r 0) b)
          (else ( gcd b r)))))
