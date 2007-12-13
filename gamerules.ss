(define (GameRules)
  (define Players '())
  
  (define (InitPlayers plyrs)
    (set! Players plyrs))
  
  (define (GetPlayer x)
    (vector-ref Players x))
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'GameRules))
  
  (λ msg
    (if (null? msg)
        (top)
        (case (car msg)
          ('InitPlayers (InitPlayers (GetParam msg 0)))
          ('GetPlayer (GetPlayer (GetParam msg 0)))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'GameRules "message not understood: ~S" (car msg)))))))