(load "cardset.ss")

(define (Player GameManager)
  (define hand (CardSet))
  
  (define (GetCard card)
    (hand 'add-card! card))
  
  (define (GiveCard card)
    (hand 'delete-card! card))
  
  (define (YourTurn card)
    (if (bDebug)
        (display 'Player "Dummy function \"YourTurn\" does not know what to do.")))
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'Player))
  
  (λ msg
    (if (null? msg)
        (first-position)
        (case (car msg)
          ('add! (add! (GetParam 0)))
          ('delete! (delete! (GetParam 0)))
          ('copyToPosList (copyToPosList))
          ('toPosList (toPosList))
          ('get-card (get-card (GetParam 0)))
          (else (error 'Player "message not understood: ~S" (car msg)))))))