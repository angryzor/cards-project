(load "humanplayer.ss")
(load "gamerules.ss")
(load "simpleguimanager.ss")

(define (GameManager PlayerNames PlayerClasses DrawerClass)
  (define GRules (GameRules))
  
  (define (InitGameRules)
    (define (CreatePlayers vec)
      (define (iter n names classes)
        (if (= n (vector-length vec))
            vec
            (begin (vector-set! vec n ((car PlayerClasses) (car names) GRules DrawerClass))
                   (iter (+ n 1) (cdr names) (cdr PlayerClasses)))))
      (iter 0 PlayerNames PlayerClasses))
    
      (GRules 'InitPlayers (CreatePlayers (make-vector (length PlayerNames)))))
  
  (InitGameRules)
  
  (λ msg
    (if (null? msg)
        (top)
        (case (car msg)
          ('GetGameRules GRules)
          (else (error 'GameRules "message not understood: ~S" (car msg)))))))