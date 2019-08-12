(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (cp pair)
  (let ((visited '()))
    (define (visited? pair vl)
        (if (or (not (pair? pair)) (memq pair visited))
            #f
            #t
      ; return #t if the pair is in vl, #f otherwise
      ; the member function is NOT useful for this because it does
      ;   not check to see if the pair being searched is the exact
      ;   same memory location as a pair in the list
      ; note that we need the second input to receive visited
      ;   so we can cdr through its contents
      ; #t is used as a placeholder for real code
      ))
    (define (cp2 pair)
      (cond ((not (pair? pair)) 0)
            ((visited? (pair? pair)) 0)
            (else (set! visited (cons pair visited))
                  (+ 1 (cp2 (car pair)) (cp2 (cdr pair)) ))))
      ; if already visited, 0
      ; else:
          ; updated visited (use set!) to include pair
          ; set! is not a placeholder--it is real code!
          ;(set! visited (cons pair visited))
          ; recursive calls on car and cdr
      ; 0 is used as a placeholder for real code
      ;0)
    (cp2 pair)))


    (define (cp pair)
  (let ((visited '()))
    (define (visited? pair vl)
      (cond ((null? vl) #f)
            ((eq? p (car l)) #f)
            (else (visited? pair (cdr l)))))
    (define (cp2 pair)
      (cond ((not (pair? pair)) 0)
            ((visited? (pair? pair)) 0)
            (else (set! visited (cons pair visited))
                  (+ 1 (cp2 (car pair)) (cp2 (cdr pair)) ))))
    (cp2 pair)))


    
(define (make-queue)
  (let ((front-ptr null)
        (rear-ptr null))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (peek)
      (cond ((empty-queue?) (error "Empty queue. :-("))
            (else (car front-ptr))))
    (define (insert-queue! datum)
      (let ((new-node (cons datum())))
        (cond ((empty-queue?)
               (set-front-ptr! new-node)
               (set-rear-ptr! new-node) dispatch)
              (else
               (set-cdr! rear-ptr new-node)
               (set-rear-ptr! new-node) dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?) (error "Empty queue. :-("))
            (else (set-front-ptr! (cdr front-ptr)) dispatch)))
    (define (dispatch message)
      (cond ((eq? message 'insert-queue!) insert-queue!)
            ((eq? message 'delete-queue!) delete-queue!)
            ((eq? message 'peek) peek)
            ((eq? message 'empty?) empty-queue?)))
    dispatch))
        
   