(load "double-linked-position-list")

(define (sorted-position-list ==? <?)
  (define p-list (position-list ==?))
  
  (define (binary-search sorted-vector element)
(define (binary-search-rec left-bound right-bound)
(if (<= left-bound right-bound)
(let ((middle (quotient (+ 1 left-bound right-bound) 2)))
(cond ((< (vector-ref sorted-vector middle) element)
;; zoek rechts van middle
(binary-search-rec (+ 1 middle) right-bound))
((> (vector-ref sorted-vector middle) element)
;; zoek links van middle
(binary-search-rec left-bound (- middle 1)))
((= (vector-ref sorted-vector middle) element)
;; gevonden
middle)
(else
#f)))
#f))
(binary-search-rec 0 (- (vector-length sorted-vector) 1)))
  
  (define (PutBeforeFind findfunc)
    (let ((pos-after (findfunc)))
      (if (null? pos-after)
          (p-list 'add-before! val)
          (p-list 'add-before! val pos-after))))
  
  ;*******************************************************************
  ; put in sorted list, and if equal,
  ; put before old value.
  ;*******************************************************************
  (define (add-before! val)
    (PutBeforeFind FindPos>=))
  
  (define (add-after! val)
    (PutBeforeFind FindPos>))
