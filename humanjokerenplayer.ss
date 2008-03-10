(load "jokerenplayer.ss")

(define (HumanJokerenPlayer Name GRules DrawerClass)
  (define (HumanJokerenPlayer-Object . msg)
    (if (null? msg)
        (error 'HumanJokerenPlayer "object requires a message")
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

  (define plyr (JokerenPlayer Name GRules DrawerClass))
  (define UI (DrawerClass GRules HumanJokerenPlayer-Object))
  
  (define (Init)
    (UI 'Init))
  
  (define (DisplayUpdate)
    (UI 'DisplayUpdate))
  
  (define (TableChanged)
    (UI 'TableChanged))
  
  (define (StatusText! str)
    (UI 'StatusText! str))

  (define (Implements? ClassDef)
    (or (eq? ClassDef 'HumanJokerenPlayer) (plyr 'Implements? ClassDef)))
  
  HumanJokerenPlayer-Object)
