(load "player.ss")

(define (HumanPlayer Name GRules DrawerClass)
  (define (HumanPlayer-Object . msg)
    (if (null? msg)
        (error 'HumanPlayer "object requires a message")
        (case (car msg)
          ('DisplayUpdate (DisplayUpdate))
          ('Init (Init))
          ('GetSelect (UI 'GetSelect))
          ('GetTableSelect (UI 'GetTableSelect))
          ('GetPlayerOwnCardsSelect (UI 'GetPlayerOwnCardsSelect))
          ('TableChanged (TableChanged))
          ('StatusText! (StatusText! (GetParam msg 0)))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (apply plyr msg)))))

  (define plyr (Player Name GRules DrawerClass))
  (define UI (DrawerClass GRules HumanPlayer-Object))
  
  (define (Init)
    (UI 'Init))
  
  (define (DisplayUpdate)
    (UI 'DisplayUpdate))
  
  (define (TableChanged)
    (UI 'TableChanged))
  
  (define (StatusText! str)
    (UI 'StatusText! str))

  (define (Implements? ClassDef)
    (or (eq? ClassDef 'HumanPlayer) (plyr 'Implements? ClassDef)))
  
  HumanPlayer-Object)
