(define (GameManager PlayerNames PlayerClass DrawerClass)
  (define GRules (GameRules))
  
  (define (InitGameRules)
    (define (CreatePlayers vec)
      (define (iter n names)
        (if (= n (vector-length vec))
            vec
            (begin (vector-set! vec n (PlayerClass (car names) GRules DrawerClass))
                   (iter (+ n 1) (cdr names)))))
      (iter 0 PlayerNames))
    
      (if (not (ObjectOfType? 'Player (PlayerClass)))
          (error 'GameManager.Constructor "PlayerClass is not a valid subclass of Player (given: ~S)" i (vector-ref Players i)))
      (GRules 'InitPlayers (CreatePlayers (make-vector (length PlayerNames)))))
  
  (InitGameRules))