(load "player.ss")

(define (HumanPlayer Name GRules DrawerClass)
  (define plyr (Player GameManager DrawerClass))
  (define UI (DrawerClass GRules))
  
  (define (DisplayUpdate)
    )
  
  (define (Implements? ClassDef)
    (or (eq? ClassDef 'HumanPlayer) (plyr 'Implements? ClassDef)))
  
  (λ msg
    (if (null? msg)
        (error 'HumanPlayer "object requires a message")
        (case (car msg)
          ('DisplayUpdate (DisplayUpdate (GetParam msg 0)))
          (else (apply plyr msg))))))