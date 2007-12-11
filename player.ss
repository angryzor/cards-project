(load "cardset.ss")

(define (Player GameManager DrawerClass)
  (define hand (CardSet))
  
  (define (ReceiveCard card)
    (hand 'add-card! card))
  
  (define (DiscardCard card)
    (hand 'delete-card! card))
  
  (define (YourTurn card)
    (if bDebug
        (display 'Player "this is an abstract class, only meant to be subclassed. Do not use it directly")))
  
  (define (DisplayUpdateTick)
    (if bDebug
        (display 'Player "this is an abstract class, only meant to be subclassed. Do not use it directly")))
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'Player))
  
  (λ msg
    (if (null? msg)
        (error 'Player "object requires a message")
        (case (car msg)
          (else (error 'Player "message not understood: ~S" (car msg)))))))