(load "cardset.ss")

(define (Player Name GRules DrawerClass)
  (define hand (CardSet))
  
  (define (Init)
    #t)
  
  (define (TableChanged)
    #t)
  
  (define (DisplayUpdate)
    #t)
  
  (define (ReceiveCard card)
    (hand 'add! card))
  
  (define (DiscardCard card)
    (hand 'delete! card)
    card)
  
  (define (YourTurn card)
    (if bDebug
        (display 'Player "this is an abstract class, only meant to be subclassed. Do not use it directly")))
  
  (define (getHand)
    hand)
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'Player))
  
  (λ msg
    (if (null? msg)
        (error 'Player "object requires a message")
        (case (car msg)
          ('Init (Init))
          ('TableChanged (TableChanged))
          ('ReceiveCard (ReceiveCard (GetParam msg 0)))
          ('DiscardCard (DiscardCard (GetParam msg 0)))
          ('YourTurn (YourTurn (GetParam msg 0)))
          ('getHand (getHand))
          ('DisplayUpdate (DisplayUpdate))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'Player "message not understood: ~S" (car msg)))))))