(load "gamerules.ss")
(load "deckgenerator.ss")

(define (PestenGameRules)
  (define Rules (GameRules))
  (define CurrentTurn 0) ; index into players indicating which player is next.
  (define TakeStack (CardStack #f))
  (define PlayStack (CardStack #t))
  (define ColorAlteration #f)

; Used to shift turns

  (define (Turn shift)
    (modulo (+ CurrentTurn shift) (Rules 'NumPlayers)))
  
;Card comparison func
  (define (CardCompare crd1 crd2)
    (if (or (eq? (crd1 'Color) (crd2 'Color))
            (= (crd1 'Value) (crd2 'Value))
            (= (crd1 'Value) 11)
            (= (crd2 'Value) 11)
            (eq? (crd1 'Value) 'Joker)
            (eq? (crd2 'Value) 'Joker))
        Card.CARD_EQUAL
        Card.CARD_HIGHER)) ; We don't care whether it's higher or lower. It doesn't matter in this game.
  
; HELPER FUNCS
  
  (define (IsJoker? crd)
    (eq? (crd 'Color) 'Joker))
  
  (define (GivePlayerCardsFromTakeStack plyr n)
    (if (> n 0)
        (begin (plyr 'ReceiveCard (TakeStack 'pop!))
               (GivePlayerCardsFromTakeStack plyr (- n 1)))))

  
;INIT FUNCS
;Creates cards
  
  (define (InitTable)
    ((Rules 'GetTable) 'add! TakeStack)
    ((Rules 'GetTable) 'add! PlayStack)
    (Rules 'SendToAllPlayers 'TableChanged))
  
  (define (CreateCards)
    (let ((deckgen (DeckGenerator CardCompare)))
      (let loop ((n 52))
        (if (> n 0)
            (begin (TakeStack 'push! (deckgen 'NextCard))
                   (loop (- n 1)))))
      (TakeStack 'push! (deckgen 'Joker))
      (TakeStack 'push! (deckgen 'Joker))))
  
; Deals cards
  
  (define (DealCards)
    (TakeStack 'shuffle)
    (let loop ((n 7))
      (if (> n 0)
          (begin (let loop2 ((plyr (- (Rules 'NumPlayers) 1)))
                   (if (>= plyr 0)
                       (begin (GivePlayerCardsFromTakeStack (Rules 'GetPlayer plyr) 1)
                              (loop2 (- plyr 1)))))
                 (loop (- n 1)))))
    (PlayStack 'push! (TakeStack 'pop!)))
  
  (define (CheckIfAPlayerHasNoCards)
    (define (iter plyr)
      (cond ((< plyr 0) #f)
            ((= ((Rules 'GetPlayer plyr) 'NumberCards) 0) plyr)
            (else (iter (- plyr 1)))))
    (iter (- (Rules 'NumPlayers) 1)))
  
; GAMEPLAY FUNCS
    
  (define (ValidMove crd)
    (let ((tmpstck (CardStack)))
      
      (define (PlaceCardsBackOn)
        (if (not (tmpstck 'empty?))
            (begin (PlayStack 'push! (tmpstck 'pop!))
                   (PlaceCardsBackOn))))
      
      (define (TakeOffCardsTillFirstNormal)
        (cond ((PlayStack 'empty?) (error 'PestenGameRules.ValidMove.@TakeOffCardsTillFirstNormal "No normal cards on stack!"))
              ((IsJoker? (PlayStack 'top)) (begin (tmpstck 'push! (PlayStack 'pop!))
                                                   (TakeOffCardsTillFirstNormal)))
              (ColorAlteration ((Card ColorAlteration 1 CardCompare) '=? crd))
              (else ((PlayStack 'top) '=? crd))))
      
      (if crd
          (let ((res (TakeOffCardsTillFirstNormal)))
            (PlaceCardsBackOn)
            res)
          #f)))
  
  
  (define (DoCardSpecialAction crd)
    (case (crd 'Value)
      ((2) (GivePlayerCardsFromTakeStack (Rules 'GetPlayer (Turn 1)) 2))
      ((0) (GivePlayerCardsFromTakeStack (Rules 'GetPlayer (Turn 1)) 5))
      ((11) (set! ColorAlteration (read))))) ; needs tweaking
  
  (define (CalcNewTurn crd)
    (case (crd 'Value)
      ((1) (Turn -1))
      ((7) (Turn 0))
      ((8) (Turn 2))
      (else (Turn 1))))
  
  (define (WaitForValidMove)
    (let ((crdsel ((Rules 'GetPlayer CurrentTurn) 'GetPlayerOwnCardsSelect)))
      (if (ValidMove crdsel)
          crdsel
          (WaitForValidMove))))
  
  (define (ProcessTurn)
    (let ((crd (WaitForValidMove)))
      (DoCardSpecialAction crd)
      (PlayStack 'push! ((Rules 'GetPlayer CurrentTurn) 'DiscardCard crd))
      (set! CurrentTurn (CalcNewTurn crd)))
    (Rules 'SendToAllPlayers 'DisplayUpdate)
    (if (not (CheckIfAPlayerHasNoCards))
        (ProcessTurn)))
  
  (define (RunRules)
    (InitTable)
    (CreateCards)
    (DealCards) ;Deal
    (Rules 'SendToAllPlayers 'DisplayUpdate)
    (ProcessTurn))
  
  (λ msg
    (if (null? msg)
        (error 'PestenGameRules "object requires a message")
        (case (car msg)
          ('RunRules (RunRules))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (apply Rules msg))))))
