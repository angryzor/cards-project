(load "double-linked-position-list")
(load "gamerules.ss")
(load "deckgenerator.ss")

(define (JokerenGameRules)
  (define Rules (GameRules))
  (define CurrentTurn 0) ; index into players indicating which player is next.
  (define TakeStack (CardStack #f))
  (define DiscardStack (CardStack #t))
  (define ConfirmStack (CardStack #t)) ; Acts as a confirm button. Since buttons are not included in 
  (define firstRound #t)
  
  ; Used to shift turns
  
  (define (Turn shift)
    (modulo (+ CurrentTurn shift) (Rules 'NumPlayers)))
  
  ;Card comparison func
  (define (CardCompare crd1 crd2)
    (cond ((or (= (crd1 'Value) (crd2 'Value))
               (eq? (crd1 'Color) 'joker)
               (eq? (crd2 'Color) 'joker)) Card.CARD_EQUAL)
          ((> (crd1 'Value) (crd2 'Value)) Card.CARD_HIGHER)
          ((< (crd1 'Value) (crd2 'Value)) Card.CARD_LOWER)))
  
  ; HELPER FUNCS
  
  (define (IsJoker? crd)
    (eq? (crd 'Color) 'joker))
  
  (define (GivePlayerCardsFromTakeStack plyr n)
    (define (MoveDiscardStackToTakeStack)
      (if (not (DiscardStack 'empty?))
          (begin (TakeStack 'push! (DiscardStack 'pop!))
                 (MoveDiscardStackToTakeStack))))
    (if (> n 0)
        (begin (if (TakeStack 'empty?)
                   (let ((topcard (DiscardStack 'pop!)))
                     (MoveDiscardStackToTakeStack)
                     (DiscardStack 'push! topcard)
                     (TakeStack 'shuffle)))
               (plyr 'ReceiveCard (TakeStack 'pop!))
               (GivePlayerCardsFromTakeStack plyr (- n 1)))))
  
  
  ;INIT FUNCS
  ;Creates cards
  
  (define (InitTable)
    ((Rules 'GetTable) 'add! TakeStack)
    ((Rules 'GetTable) 'add! DiscardStack)
    ((Rules 'GetTable) 'add! ConfirmStack)
    (Rules 'SendToAllPlayers 'TableChanged))
  
  (define (CreateCards)
    (let ((deckgen (DeckGenerator CardCompare)))
      (let loop ((n 104))
        (if (> n 0)
            (begin (TakeStack 'push! (deckgen 'NextCard))
                   (loop (- n 1)))))
      (TakeStack 'push! (deckgen 'Joker))
      (TakeStack 'push! (deckgen 'Joker))
      (ConfirmStack 'push! (deckgen 'Joker))))
  
  ; Deals cards
  
  (define (DealCards)
    (TakeStack 'shuffle)
    (let loop ((n 13))
      (if (> n 0)
          (begin (let loop2 ((plyr (- (Rules 'NumPlayers) 1)))
                   (if (>= plyr 0)
                       (begin (GivePlayerCardsFromTakeStack (Rules 'GetPlayer plyr) 1)
                              (loop2 (- plyr 1)))))
                 (loop (- n 1)))))
    (DiscardStack 'push! (TakeStack 'pop!)))
  
  (define (PlayerHasNoCards? plyr)
    (= (plyr 'NumberCards) 0))
  
  (define (CheckIfAPlayerHasNoCards)
    (define (iter plyr)
      (cond ((< plyr 0) #f)
            ((PlayerHasNoCards? (Rules 'GetPlayer plyr)) plyr)
            (else (iter (- plyr 1)))))
    (iter (- (Rules 'NumPlayers) 1)))
  
  ; GAMEPLAY FUNCS
  
  (define (OriginatesFromCurrentPlayer crdsel)
    (eq? (crdsel 'Origin) ((Rules 'GetPlayer CurrentTurn) 'getHand)))
  
  (define (OriginatesFromTakeStack crdsel)
    (eq? (crdsel 'Origin) TakeStack))
  
  (define (OriginatesFromDiscardStackStack crdsel)
    (eq? (crdsel 'Origin) DiscardStack))
  
  (define (GetFirstNormal)
    (let ((tmpstck (CardStack)))
      
      (define (PlaceCardsBackOn)
        (if (not (tmpstck 'empty?))
            (begin (DiscardStack 'push! (tmpstck 'pop!))
                   (PlaceCardsBackOn))))
      
      (define (TakeOffCardsTillFirstNormal)
        (cond ((DiscardStack 'empty?) (error 'PestenGameRules.ValidMove.@TakeOffCardsTillFirstNormal "No normal cards on stack!"))
              ((IsJoker? (DiscardStack 'top)) (begin (tmpstck 'push! (DiscardStack 'pop!))
                                                     (TakeOffCardsTillFirstNormal)))
              (else (DiscardStack 'top))))
      
      (let ((res (TakeOffCardsTillFirstNormal)))
        (PlaceCardsBackOn)
        res)))
  
  (define (WaitForValidMove noTakeStackOrigin)
    (let ((crdsel ((Rules 'GetPlayer CurrentTurn) 'GetSelect)))
      (cond ((not crdsel) (WaitForValidMove noTakeStackOrigin))
            ((and (OriginatesFromCurrentPlayer crdsel)
                  (ValidMove (crdsel 'Card))) crdsel)
            ((and (not noTakeStackOrigin)
                  (OriginatesFromTakeStack crdsel)) crdsel)
            (else (WaitForValidMove noTakeStackOrigin)))))
  
  (define (WaitForSelection . possibleorigins)
    (define (TryOrigins sel)
      (define (iter orgns)
        (cond ((null? orgns) #f)
              ((eq? (sel 'Origin) (car orgns)) #t)
              (else (iter (cdr orgns)))))
      (iter possibleorigins))
    (let ((sel ((Rules 'GetPlayer CurrentTurn) 'GetSelect)))
      (if (TryOrigins sel)
          sel
          (apply WaitForSelection possibleorigins))))
  
  
  
  (define (PosListAddSorting lst val)
    (define (iter pos)
      (cond ((val '<=? (lst 'value pos)) (lst 'add-before! val pos))
            ((not (lst 'has-next? pos)) (lst 'add-after! val))
            (else (iter (lst 'next pos)))))
    (if (not (lst 'empty?))
        (iter (lst 'first-position))
        (lst 'add-after! val)))
  
  (define (WaitForSetBuild set-lst)
    (let* ((thisP (Rules 'GetPlayer CurrentTurn))
            (sel (WaitForSelection (thisP 'getHand) ConfirmStack)))
      (if (eq? (sel 'Origin) (thisP 'getHand))
          (begin
            (thisP 'DiscardCard (sel 'Card))
            (PosListAddSorting set-lst (sel 'Card))
            (thisP 'DisplayUpdate)
            (WaitForSetBuild set-lst))
          (Check)))
  
;  (define (EnterCardManagement)
;    )
  
  (define (ProcessTurn)
    (let ((thisP (Rules 'GetPlayer CurrentTurn)))
      (let ((sel (WaitForSelection TakeStack DiscardStack)))
        (cond ((eq? (sel 'Origin) TakeStack) (if (and firstRound (= CurrentTurn 1))
                                                 (set! firstRound #f))
                                             (thisP 'ReceiveCard (TakeStack 'pop!))) ; Player takes card from takestack
              ((and firstRound (= CurrentTurn 1)) (set! firstRound #f)
                                                  (thisP 'ReceiveCard (TakeStack 'pop!)))
              (else (thisP 'add! (sel 'Card))
                    (CheckIfAtLeast40 thisP)
                    (blah)))) ; TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
      (Rules 'SendToAllPlayers 'DisplayUpdate)
      (let loop ((sel (WaitForSelection (thisP 'getHand)))
                 (set (CardSet #t)))
        (display "2nd proct")
        ((Rules 'GetTable) 'add! set)
        (Rules 'SendToAllPlayers 'TableChanged)
        ((Rules 'GetPlayer CurrentTurn) 'DisplayUpdate)
        (WaitForSetBuild (set 'toPosList))
        (Rules 'SendToAllPlayers 'DisplayUpdate))))
  
  
  

(define (LoopThroughTurns)
  (ProcessTurn)
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
  (display "Spel beëindigd"))

(λ msg
  (if (null? msg)
      (error 'PestenGameRules "object requires a message")
      (case (car msg)
        ('RunRules (RunRules))
        ('Implements? (Implements? (GetParam msg 0)))
        (else (apply Rules msg))))))
