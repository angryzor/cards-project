(load "gamerules.ss")
(load "deckgenerator.ss")

(define (PestenGameRules)
  (define Rules (GameRules))
  (define CurrentTurn 0) ; index into players indicating which player is next.
  (define TakeStack (CardStack #f))
  (define PlayStack (CardStack #t))
  (define ColorStack (CardStack #t))
  (define ColorAlteration #f)
  (define turnDir 1)
  (define cardTakeAcc 0)
  
  ; Used to shift turns
  
  (define (Turn shift)
    (modulo (+ CurrentTurn shift) (Rules 'NumPlayers)))
  
  ;Card comparison func
  (define (CardCompare crd1 crd2)
    (if (or (eq? (crd1 'Color) (crd2 'Color))
            (= (crd1 'Value) (crd2 'Value))
            (= (crd1 'Value) 11)
            (= (crd2 'Value) 11)
            (eq? (crd1 'Color) 'joker)
            (eq? (crd2 'Color) 'joker))
        Card.CARD_EQUAL
        Card.CARD_HIGHER)) ; We don't care whether it's higher or lower. It doesn't matter in this game.
  
  ; HELPER FUNCS
  
  (define (IsJoker? crd)
    (eq? (crd 'Color) 'joker))
  
  (define (GivePlayerCardsFromTakeStack plyr n)
    (define (MovePlayStackToTakeStack)
      (if (not (PlayStack 'empty?))
          (begin (TakeStack 'push! (PlayStack 'pop!))
                 (MovePlayStackToTakeStack))))
    (if (> n 0)
        (begin (if (TakeStack 'empty?)
                   (let ((topcard (PlayStack 'pop!)))
                     (MovePlayStackToTakeStack)
                     (PlayStack 'push! topcard)
                     (TakeStack 'shuffle)))
               (plyr 'ReceiveCard (TakeStack 'pop!))
               (GivePlayerCardsFromTakeStack plyr (- n 1)))))
  
  
  ;INIT FUNCS
  ;Creates cards
  
  (define (InitTable)
    ((Rules 'GetTable) 'add! TakeStack)
    ((Rules 'GetTable) 'add! PlayStack)
    ((Rules 'GetTable) 'add! ColorStack)
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
  
  (define (RetryTillNoJoker crd)
    (define (rec crd)
      (if (crd 'equal? (Card 'joker 0 CardCompare))
          (let ((res (RetryTillNoJoker (TakeStack 'pop!))))
            (TakeStack 'push! crd)
            res)
          crd))
    (rec crd))
  
  (define (DealCards)
    (TakeStack 'shuffle)
    (let loop ((n 7))
      (if (> n 0)
          (begin (let loop2 ((plyr (- (Rules 'NumPlayers) 1)))
                   (if (>= plyr 0)
                       (begin (GivePlayerCardsFromTakeStack (Rules 'GetPlayer plyr) 1)
                              (loop2 (- plyr 1)))))
                 (loop (- n 1)))))
    (PlayStack 'push! (RetryTillNoJoker (TakeStack 'pop!))))
  
  (define (CheckIfAPlayerHasNoCards)
    (define (iter plyr)
      (cond ((< plyr 0) #f)
            ((= ((Rules 'GetPlayer plyr) 'NumberCards) 0) plyr)
            (else (iter (- plyr 1)))))
    (iter (- (Rules 'NumPlayers) 1)))
  
  ; GAMEPLAY FUNCS
  
  (define (OriginatesFromCurrentPlayer crdsel)
    (eq? (crdsel 'Origin) ((Rules 'GetPlayer CurrentTurn) 'getHand)))
  
  (define (OriginatesFromTakeStack crdsel)
    (eq? (crdsel 'Origin) TakeStack))
  
  (define (PlayerHasNoValidCards plyr)
    (let ((lst ((plyr 'getHand) 'toPosList)))
      (define (iter pos)
        (cond ((ValidMove (lst 'value pos)) #f)
              ((not (lst 'has-next? pos)) #t)
              (else (iter (lst 'next pos)))))
      (iter (lst 'first-position))))
  
  
  
  (define (GetFirstNormal)
    (let ((tmpstck (CardStack)))
      
      (define (PlaceCardsBackOn)
        (if (not (tmpstck 'empty?))
            (begin (PlayStack 'push! (tmpstck 'pop!))
                   (PlaceCardsBackOn))))
      
      (define (TakeOffCardsTillFirstNormal)
        (cond ((PlayStack 'empty?) (error 'PestenGameRules.ValidMove.@TakeOffCardsTillFirstNormal "No normal cards on stack!"))
              ((IsJoker? (PlayStack 'top)) (begin (tmpstck 'push! (PlayStack 'pop!))
                                                  (TakeOffCardsTillFirstNormal)))
              (else (PlayStack 'top))))
      
      (let ((res (TakeOffCardsTillFirstNormal)))
        (PlaceCardsBackOn)
        res)))
  
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
      
      (let ((res (TakeOffCardsTillFirstNormal)))
        (PlaceCardsBackOn)
        res)))
  
  
  (define (DoCardSpecialAction crd)
    (case (crd 'Value)
      ((2) (set! cardTakeAcc (+ cardTakeAcc 2))
           (set! ColorAlteration #f))
      ((0) (set! cardTakeAcc (+ cardTakeAcc 5))
           (set! ColorAlteration #f))
      ((11) (display "Geef de kleur waarnaar u wilt wijzigen op a.u.b.")
            (newline)
            (set! ColorAlteration (read))
            (newline))
      (else (set! ColorAlteration #f))))
  
  (define (CalcNewTurn crd)
    (case (crd 'Value)
      ((1) (set! turnDir (- turnDir))
           (Turn turnDir))
      ((7) (Turn 0))
      ((8) (Turn (* turnDir 2)))
      (else (Turn turnDir))))
  
  (define (CanPassAccumulatingCard? player)
    (let* ((hand (player 'getHand))
           (lst (hand 'toPosList)))
      (define (iter pos)
        (cond ((and (or (= ((lst 'value pos) 'Value) 1)
                        (= ((lst 'value pos) 'Value) 2))
                    (ValidMove (lst 'value pos))) #t)
              ((eq? ((lst 'value pos) 'Color) 'joker) #t)
              ((lst 'has-next? pos) (iter (lst 'next pos)))
              (else #f)))
      (iter (lst 'first-position))))
  
  (define (AccumulatingCard? crd)
    (or (= (crd 'Value) 1)
        (= (crd 'Value) 2)
        (= (crd 'Value) 0)))
  
  (define (WaitForValidMove noTakeStackOrigin shouldBeAcc)
    (let ((crdsel ((Rules 'GetPlayer CurrentTurn) 'GetSelect)))
      (cond ((not crdsel) (WaitForValidMove noTakeStackOrigin shouldBeAcc))
            ((and (not shouldBeAcc)
                  (OriginatesFromCurrentPlayer crdsel)
                  (ValidMove (crdsel 'Card))) crdsel)
            ((and (OriginatesFromCurrentPlayer crdsel)
                  (ValidMove (crdsel 'Card))
                  (AccumulatingCard? (crdsel 'Card))) crdsel)
            ((and (not noTakeStackOrigin)
                  (OriginatesFromTakeStack crdsel)) crdsel)
            (else (WaitForValidMove noTakeStackOrigin shouldBeAcc)))))
  
  (define (ProcessTurn afterTaking)
    (let ((skip #f))
      (if (not (= cardTakeAcc 0))
          (if (CanPassAccumulatingCard? (Rules 'GetPlayer CurrentTurn))
              (begin ((Rules 'GetPlayer CurrentTurn) 'StatusText! "Klik op een kaart om de pestkaart door te geven of klik op de afneemstapel om de kaarten op te nemen.")
                     ((Rules 'GetPlayer CurrentTurn) 'DisplayUpdate)
                     (let ((crdsel (WaitForValidMove #f #t)))
                       (if (OriginatesFromTakeStack crdsel)
                           (begin (GivePlayerCardsFromTakeStack (Rules 'GetPlayer CurrentTurn) cardTakeAcc)
                                  (set! cardTakeAcc 0))
                           (begin (PlayStack 'push! ((Rules 'GetPlayer CurrentTurn) 'DiscardCard (crdsel 'Card)))
                                  (DoCardSpecialAction (crdsel 'Card))
                                  (set! CurrentTurn (CalcNewTurn (crdsel 'Card)))
                                  (set! skip #t)))))
              (begin (GivePlayerCardsFromTakeStack (Rules 'GetPlayer CurrentTurn) cardTakeAcc)
                     (set! cardTakeAcc 0))))
      (if (not skip)
          (if (and afterTaking
                   (PlayerHasNoValidCards (Rules 'GetPlayer CurrentTurn)))
              (set! CurrentTurn (Turn 1))
              (let ((crdsel (WaitForValidMove afterTaking #f)))
                (if (OriginatesFromTakeStack crdsel)
                    (begin ; Player takes card from stack
                      (GivePlayerCardsFromTakeStack (Rules 'GetPlayer CurrentTurn) 1)
                      (Rules 'SendToAllPlayers 'DisplayUpdate)
                      (ProcessTurn #t))
                    (begin ; Player plays card
                      (DoCardSpecialAction (crdsel 'Card))
                      (PlayStack 'push! ((Rules 'GetPlayer CurrentTurn) 'DiscardCard (crdsel 'Card)))
                      (set! CurrentTurn (CalcNewTurn (crdsel 'Card))))))))
      (DispCurrentColor)
      (Rules 'SendToAllPlayers 'StatusText! (string-append (symbol->string ((Rules 'GetPlayer CurrentTurn) 'Name)) " is aan de beurt."))
      (Rules 'SendToAllPlayers 'DisplayUpdate)))
  
  (define (DispCurrentColor)
    (ColorStack 'pop!)
    (if ColorAlteration
        (ColorStack 'push! (Card ColorAlteration 1 CardCompare))
        (ColorStack 'push! (Card ((GetFirstNormal) 'Color) 1 CardCompare))))
  
  (define (LoopThroughTurns)
    (ProcessTurn #f)
    (let ((plyrwocards (CheckIfAPlayerHasNoCards)))
      (if (not plyrwocards)
          (LoopThroughTurns)
          (begin (display (plyrwocards 'Name) " heeft het spel gewonnen!")
                 (newline)))))
  
  (define (RunRules)
    (InitTable)
    (CreateCards)
    (DealCards) ;Deal
    (Rules 'SendToAllPlayers 'DisplayUpdate)
    (LoopThroughTurns)
    (display "Einde spel"))
  
  (λ msg
    (if (null? msg)
        (error 'PestenGameRules "object requires a message")
        (case (car msg)
          ('RunRules (RunRules))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (apply Rules msg))))))
