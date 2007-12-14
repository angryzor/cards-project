(load "global.ss")

(define (CardTable)
  (define plst (position-list eq?))
  
  (define (add! container)
    (if (ObjectOfType? 'CardContainer container)
        (plst 'add-after! container)
        (error 'CardTable.add! "expects type <CardContainer> as first argument, given: ~S" container)))
  
  (define (remove! container)
    (if (ObjectOfType? 'CardContainer container)
        (plst 'delete! (plst.find container))
        (error 'CardTable.add! "expects type <CardContainer> as first argument, given: ~S" container)))
  
  (define (toPosList)
    plst)
  
  (define (copyToPosList)
    (plst 'duplicate))
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'CardTable))
  
  (λ msg
    (if (null? msg)
        (error 'CardTable "object requires a message")
        (case (car msg)
          ('add! (add! (GetParam msg 0)))
          ('remove! (remove! (GetParam msg 0)))
          ('toPosList (toPosList))
          ('copyToPosList (copyToPosList))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'CardTable "message not understood: ~S" (car msg)))))))