(load "player.ss")

(define (HumanPlayer GameManager DrawerClass)
  (define plyr (Player GameManager DrawerClass))
  
  (define (Implements? ClassDef)
    (or (eq? ClassDef 'HumanPlayer) (plyr 'Implements? ClassDef))))