(define (GameManager PlayerNames PlayerClasses DrawerClass)
  (define GRules (GameRules))
  
  (define (InitGameRules)
    (define (CreatePlayers vec)
      (define (iter n names classes)
        (if (= n (vector-length vec))
            vec
            (begin (vector-set! vec n ((car PlayerClasses) (car names) GRules DrawerClass))
                   (iter (+ n 1) (cdr names) (cdr PlayerClasses)))))
      (iter 0 PlayerNames))
    
      (if (not (ObjectOfType? 'Player (PlayerClass)))
          (error 'GameManager.Constructor "PlayerClass is not a valid subclass of Player (given: ~S)" i (vector-ref Players i)))
      (GRules 'InitPlayers (CreatePlayers (make-vector (length PlayerNames)))))
  
  (InitGameRules))