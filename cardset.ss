(load "double-linked-position-list.ss")

(define (CardSet)
  (define (Card-ComparisonCaller x y)
    (x '=? y))
  (define plst (position-list Card-ComparisonCaller))
  
  (define (add! card)
    (if (ObjectOfType? 'Card card)
        (plst 'add-after! card)
        (error 'CardSet.add! "expects type <Card> as argument, given: ~S" card)))
  
  (define (delete! card)
    (if (ObjectOfType? 'Card card)
        (plst 'delete! (plst 'find card))
        (error 'CardSet.delete! "expects type <Card> as argument, given: ~S" card)))
  
  (define (copyToPosList)
    (plst 'duplicate))
  
  (define (toPosList)
    plst)
  
  (define (get-card card)
    (plst 'find card))
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'CardSet))
  
  (λ msg
    (if (null? msg)
        (first-position)
        (case (car msg)
          ('add! (add! (GetParam msg 0)))
          ('delete! (delete! (GetParam msg 0)))
          ('copyToPosList (copyToPosList))
          ('toPosList (toPosList))
          ('get-card (get-card (GetParam msg 0)))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'CardSet "message not understood: ~S" (car msg)))))))
