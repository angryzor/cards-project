(define (GameRules)
  (define Players '())
  
  (define (InitPlayers plyrs)
    (set! Players plyrs)
    (CheckPlayers))
  
  (define (GetPlayer x)
    (vector-ref Players x))
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'GameRules))
  
  (define (CheckPlayers)
    (define (iter plyrs)
      (cond ((null? plyrs) #t)
            ((ObjectOfType? 'Player (car plyrs)) (iter (cdr plyrs)))
            (else (error 'GameRules.Constructor "PlayerClass is not a valid subclass of Player (given: ~S)" (car plyrs)))))
    (iter Players))
  
  (λ msg
    (if (null? msg)
        (top)
        (case (car msg)
          ('InitPlayers (InitPlayers (GetParam msg 0)))
          ('GetPlayer (GetPlayer (GetParam msg 0)))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'GameRules "message not understood: ~S" (car msg)))))))