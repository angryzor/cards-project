(define (GameRules)
  (define Players '())
  
  (define (InitPlayers plyrs)
    (set! Players plyrs))
  
  (define (GetPlayer x)
    (vector-ref Players x))
  
  