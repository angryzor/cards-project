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
    
    (let ((Players (CreatePlayers (make-vector (length PlayerNames)))))
      (define (CheckPlayers)
        (define (iter i)
          (cond ((= i (vector-length Players)) #t)
                ((ObjectOfType? 'Player (vector-ref Players i)) (iter (+ i 0)))
                (else (error 'GameManager.Constructor "PlayerClass is not a valid subclass of Player (given: ~S)" i (vector-ref Players i)))))
        (iter 0))
      (CheckPlayers)
      (GRules 'InitPlayers Players)))
  
  (InitGameRules)
  )