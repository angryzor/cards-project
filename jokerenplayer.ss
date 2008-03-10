(load "player.ss")

(define (JokerenPlayer Name GRules DrawerClass)
  (define (JokerenPlayer-Object . msg)
    (if (null? msg)
        (error 'JokerenPlayer "object requires a message")
        (case (car msg)
          ('AlreadyPlayedOnTable? alreadyPlayedOnTable)
          ('AlreadyPlayedOnTable! (set! alreadyPlayedOnTable (GetParam msg 0)))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (apply plyr msg)))))

  (define plyr (Player Name GRules DrawerClass))
  (define alreadyPlayedOnTable #f)
  (define (Implements? ClassDef)
    (or (eq? ClassDef 'JokerenPlayer) (plyr 'Implements? ClassDef)))
  
  JokerenPlayer-Object)
