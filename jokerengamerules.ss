(load "double-linked-position-list.ss")
(load "gamerules.ss")
(load "deckgenerator.ss")
(load "debug.ss")

(define (JokerenGameRules)
  (define Rules (GameRules))
  (define CurrentTurn 0) ; index into players indicating which player is next.
  (define TakeStack (CardStack #f))
  (define DiscardStack (CardStack #t))
  (define ConfirmStack (CardStack #t)) ; Acts as a confirm button.
  (define firstRound #t)
  (define SetsOnTable (position-list eq?))
  
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
      (TakeStack 'push! (deckgen 'Joker))
      (TakeStack 'push! (deckgen 'Joker))
      (ConfirmStack 'push! (deckgen 'Joker))))
  
  ; Deals cards
  
  (define (DealCards)
;    (TakeStack 'shuffle)
;    (let loop ((n 13))
;      (if (> n 0)
;          (begin (let loop2 ((plyr (- (Rules 'NumPlayers) 1)))
;                   (if (>= plyr 0)
;                       (begin (GivePlayerCardsFromTakeStack (Rules 'GetPlayer plyr) 1)
;                              (loop2 (- plyr 1)))))
;                 (loop (- n 1)))))
    (let loop2 ((plyr (- (Rules 'NumPlayers) 1)))
      (if (>= plyr 0)
          (begin (let loop ((n 13))
                   (if (> n 0)
                       (begin (GivePlayerCardsFromTakeStack (Rules 'GetPlayer plyr) 1)
                              (loop (- n 1)))))
                 (loop2 (- plyr 1)))))
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
  
  (define (OriginatesFromCurrentPlayer? crdsel)
    (eq? (crdsel 'Origin) ((Rules 'GetPlayer CurrentTurn) 'getHand)))
  
  (define (OriginatesFromTakeStack? crdsel)
    (eq? (crdsel 'Origin) TakeStack))
  
  (define (OriginatesFromDiscardStack? crdsel)
    (eq? (crdsel 'Origin) DiscardStack))
  
  (define (OriginatesFromConfirmStack? crdsel)
    (eq? (crdsel 'Origin) ConfirmStack))

  (define (WaitForSelection . possibleorigins)
    (define (TryOrigins sel)
      (define (iter orgns)
        (cond ((null? orgns) #f)
              ((eq? (sel 'Origin) (car orgns)) #t)
              (else (iter (cdr orgns)))))
      (iter possibleorigins))
    (let ((sel ((Rules 'GetPlayer CurrentTurn) 'GetSelect)))
      (if (and sel
               (TryOrigins sel))
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
  
  (define (RefundCards set-lst)
    (define (iter pos)
      ((Rules 'GetPlayer CurrentTurn) 'ReceiveCard (set-lst 'value pos))
      (if (set-lst 'has-next? pos)
          (iter (set-lst 'next pos))))
    (if (not (set-lst 'empty?)) (iter (set-lst 'first-position))))
  
; Operations on the set builds
  
  (define (RefundAllCards setbuilds)
    (map (λ (x)
           (RefundCards (x 'toPosList))) setbuilds))
  
  (define (TotalValue setbuilds)
    (foldl (λ (x y)
             (+ y ((x 'toPosList) 'foldl (λ (x y)
                                           (+ x (cond ((or (= 1 (y 'Value))
                                                           (= 11 (y 'Value))
                                                           (= 12 (y 'Value))
                                                           (= 13 (y 'Value))) 10)
                                                      ((= 0 (y 'Value)) 25)
                                                      (else (y 'Value))))) 0))) 0 setbuilds))
  
  (define (RemoveSetBuildsFromTable setbuilds)
    (for-each (λ (x)
                ((Rules 'GetTable) 'remove! x)) setbuilds))
  
  
; Check if a card is valid
  
  (define (CheckIfValidSet set-lst)
    (define (CheckIfSet)
      (define (NoDoubles?)
        (let ((colors (position-list eq?)))
          (define (iter pos)
            (let ((colpos (colors 'find ((set-lst 'value pos) 'Color))))
              (cond (colpos (colors 'delete! colpos)
                            (if (set-lst 'has-next? pos)
                                (iter (set-lst 'next pos))
                                #t))
                    ((eq? ((set-lst 'value pos) 'Color) 'joker) (if (set-lst 'has-next? pos)
                                                                    (iter (set-lst 'next pos))
                                                                    #t))
                    (else #f))))
          (colors 'add-after! 'klaveren)
          (colors 'add-after! 'harten)
          (colors 'add-after! 'ruiten)
          (colors 'add-after! 'schoppen)
          (iter (set-lst 'first-position))))
      (define (AllSameNumber?)
        (define (iter pos number)
          (if (= number 0)
              (set! number ((set-lst 'value pos) 'Value)))
          (cond ((and (not (= number ((set-lst 'value pos) 'Value)))
                      (not (= ((set-lst 'value pos) 'Value) 0))) #f)
                ((set-lst 'has-next? pos) (iter (set-lst 'next pos) number))
                (else #t)))
        (iter (set-lst 'first-position) ((set-lst 'value (set-lst 'first-position)) 'Value)))
      (and (AllSameNumber?)
           (NoDoubles?)))
    (define (CheckIfRow)
      (define (iter pos curnum col)
        (if (= curnum 0)
            (set! curnum ((set-lst 'value pos) 'Value)))
        (if (eq? col 'joker)
            (set! col ((set-lst 'value pos) 'Color)))
        (cond ((or (and (not (or (= curnum ((set-lst 'value pos) 'Value))
                                 (and (= curnum 14)
                                      (= ((set-lst 'value pos) 'Value) 1))))
                        (not (= ((set-lst 'value pos) 'Value) 0)))
                   (and (not (eq? col ((set-lst 'value pos) 'Color)))
                        (not (eq? ((set-lst 'value pos) 'Color) 'joker)))) #f)
              ((set-lst 'has-next? pos) (iter (set-lst 'next pos) (+ curnum 1) col))
              (else #t)))
      (iter (set-lst 'first-position) ((set-lst 'value (set-lst 'first-position)) 'Value) ((set-lst 'value (set-lst 'first-position)) 'Color)))
    (and (>= (set-lst 'length) 3)
         (or (CheckIfSet)
             (CheckIfRow))))
  
  (define (WaitForSetBuild set-lst)
    (let* ((thisP (Rules 'GetPlayer CurrentTurn))
           (sel (WaitForSelection (thisP 'getHand) ConfirmStack)))
      (cond ((eq? (sel 'Origin) (thisP 'getHand)) (thisP 'DiscardCard (sel 'Card))
                                                  (set-lst 'add-after! (sel 'Card))
                                                  (thisP 'DisplayUpdate)
                                                  (WaitForSetBuild set-lst))
            ((CheckIfValidSet set-lst) #t)
            (else (RefundCards set-lst)
                  #f))))
  
  (define (MainTurnTime setbuilds cardsmusthave)
    (let ((thisP (Rules 'GetPlayer CurrentTurn)))
      (thisP 'StatusText! "Klik op de kaarten in uw hand om te beginnen met het vormen van een set/rij of klik op de joker om uw beurt te beëindigen.")
      (thisP 'DisplayUpdate)
      (let loop ((sel (apply WaitForSelection (thisP 'getHand) ConfirmStack (SetsOnTable 'to-scheme-list))))
        (cond ((OriginatesFromCurrentPlayer? sel) (let ((set (CardSet #t)))
                                                    ((Rules 'GetTable) 'add! set)
                                                    (Rules 'SendToAllPlayers 'TableChanged)
                                                    (thisP 'StatusText! "Klik op de kaarten die u wilt toevoegen aan de nieuwe set/rij. Klik op de joker om te bevestigen.")
                                                    (thisP 'DisplayUpdate)
                                                    (if (not (WaitForSetBuild (set 'toPosList)))
                                                        (begin
                                                          ((Rules 'GetTable) 'remove! set)
                                                          (Rules 'SendToAllPlayers 'TableChanged)
                                                          (MainTurnTime setbuilds cardsmusthave))
                                                        (begin
                                                          (SetsOnTable 'add-after! set)
                                                          (MainTurnTime (cons set setbuilds) cardsmusthave)))))
              ((OriginatesFromConfirmStack? sel) (if (and (not (null? setbuilds)) (not (thisP 'AlreadyPlayedOnTable?)) (< (TotalValue setbuilds) 40))
                                                     (begin
                                                       (RefundAllCards setbuilds)
                                                       (RemoveSetBuildsFromTable setbuilds)
                                                       (Rules 'SendToAllPlayers 'TableChanged)
                                                       (MainTurnTime '() '()))
                                                     (begin
                                                       (thisP 'AlreadyPlayedOnTable! #t)
                                                       (thisP 'StatusText! "Kies een kaart die u op de aflegstapel wilt plaatsen.")
                                                       (thisP 'DisplayUpdate))))
              (else (begin 
                      (if (thisP 'AlreadyPlayedOnTable?)
                          (begin
                            (thisP 'StatusText! "Klik op de kaart (in uw hand) die u wilt toevoegen aan deze set/rij.")
                            (thisP 'DisplayUpdate)
                            (let ((lstsel ((sel 'Origin) 'copyToPosList))
                                  (csel (WaitForSelection (thisP 'getHand))))
                              (PosListAddSorting lstsel (csel 'Card))
                              (if (CheckIfValidSet lstsel)
                                  (begin
                                    (PosListAddSorting ((sel 'Origin) 'toPosList) (csel 'Card))
                                    (thisP 'DiscardCard (csel 'Card)))))))
                      (MainTurnTime setbuilds cardsmusthave)))))))
  
  
  (define (ProcessTurn)
    (let ((thisP (Rules 'GetPlayer CurrentTurn)))
      (Rules 'SendToAllPlayersBut thisP 'StatusText! (string-append (symbol->string (thisP 'Name)) " is aan de beurt."))
      (thisP 'StatusText! "U bent aan de beurt. Klik op de afneemstapel om een kaart te nemen.")
      (Rules 'SendToAllPlayers 'DisplayUpdate)
      (let ((sel (WaitForSelection TakeStack DiscardStack)))
        (cond ((eq? (sel 'Origin) TakeStack) (if (and firstRound (= CurrentTurn 1))
                                                 (set! firstRound #f))
                                             (thisP 'ReceiveCard (TakeStack 'pop!))) ; Player takes card from takestack
              ((and firstRound (= CurrentTurn 1)) (set! firstRound #f)
                                                  (thisP 'ReceiveCard (TakeStack 'pop!)))
              (else (thisP 'ReceiveCard (sel 'Card))))) ; TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
      (Rules 'SendToAllPlayers 'DisplayUpdate)
      (MainTurnTime '() '())
      (let ((sel (WaitForSelection (thisP 'getHand))))
        (thisP 'DiscardCard (sel 'Card))
        (DiscardStack 'push! (sel 'Card)))
      (set! CurrentTurn (Turn 1))))
  
  
  
  
  (define (LoopThroughTurns)
    (ProcessTurn)
    (let ((plyrwocards (CheckIfAPlayerHasNoCards)))
      (if (not plyrwocards)
          (LoopThroughTurns)
          (begin (Rules 'SendToAllPlayers 'DisplayUpdate)
                 (display (plyrwocards 'Name) " heeft het spel gewonnen!")
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
