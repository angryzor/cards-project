(define (sorted-position-list ==? <?)
  (define p-list (position-list ==?))
  
  (define (add-before-override