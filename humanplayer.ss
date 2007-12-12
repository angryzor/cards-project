(load "player.ss")

(define (HumanPlayer GameManager DrawerClass)
  (define plyr (Player GameManager DrawerClass))
  
  (define (Implements? ClassDef)
    (or (eq? ClassDef 'HumanPlayer) (plyr 'Implements? ClassDef)))
  
  (λ msg
    (if (null? msg)
        (error 'HumanPlayer "object requires a message")
        (case (car msg)
          (else (apply plyr msg))))))