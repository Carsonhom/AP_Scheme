(define meter1
   (let ((amount 0))
      (lambda ()
         (lambda (m)
            (cond ((equal? m 'deposit)
                     (set! amount (+ amount .25)))
                  ((equal? m 'total) amount)
                  ((equal? m 'collect)
                     (let ((amt amount))
                        (set! amount 0)
                        amt))
                  (else 'Eh?))))))
                   

(define meter2
   (lambda ()
      (let ((amount 0))
         (lambda (m)
            (cond ((equal? m 'deposit)
                     (set! amount (+ amount .25)))
                  ((equal? m 'total) amount)
                  ((equal? m 'collect)
                     (let ((amt amount))
                        (set! amount 0)
                        amt))
                  (else 'Eh?))))))


(define p1 (meter1))
(define p2 (meter1))

(define q1 (meter2))
(define q2 (meter2))

(define meter3
   (let ((total-amount 0))
      (lambda ()
        (let ((amount 0))
          (lambda (m)
            (cond ((equal? m 'deposit)
                   (set! amount (+ amount .25))
                   (set! total-amount (+ total-amount .25)))
                  ((equal? m 'amount) amount)
                  ((equal? m 'total) total-amount)
                  ((equal? m 'collect)
                   (let ((amt amount))
                     (set! total-amount (- total-amount amount))
                     (set! amount 0)
                     amt))
                  (else 'Eh?)))))))

(define a1 (meter3))
(define a2 (meter3))