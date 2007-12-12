; Globals that come in handy for oop

(define (GetParam msg n)
  (define (iter m ctr)
    (cond ((null? m) (error 'GetParam "Not enough params passed to message ~S (needed at least ~S, given ~S)" (car msg) n (- n ctr)))
          ((= ctr 0) (car m))
          (else (iter (cdr m) (- ctr 1)))))
  (iter (cdr msg) n))

