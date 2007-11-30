; Globals that come in handy for oop

;FIXME: Might cause overhead as is
;SOLUTION: On receiving of lessage, convert to vector.
(define (GetParam msg n)
  (define (iter m ctr)
    ; FIXME: Maybe add msg? check here?
    (cond ((null? m) (error 'GetParam "Not enough params passed to message ~S (needed at least ~S, given ~S)" (car msg) n (- n ctr)))
          ((= n 0) (car m))
          (else (iter (cdr m) (- n 1)))))
  (iter (cdr msg) n))

