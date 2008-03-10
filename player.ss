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
    (if (hand 'delete! card)
        card
        #f))
  
  (define (HasCard? card)
    (not (= (hand 'get-card card) #f)))
  
  (define (NumberCards)
    (hand 'length))
  
  (define (getHand)
    hand)
  
  (define (StatusText! str)
    #t)
  
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
          ('HasCard? (HasCard? (GetParam msg 0)))
          ('NumberCards (NumberCards))
          ('getHand (getHand))
          ('DisplayUpdate (DisplayUpdate))
          ('GetSelect #f)
          ('GetTableSelect #f)
          ('GetPlayerOwnCardsSelect #f)
          ('StatusText! (StatusText! (GetParam msg 0)))
          ('Name Name)
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'Player "message not understood: ~S" (car msg)))))))