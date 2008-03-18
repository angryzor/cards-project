(load "humanplayer.ss")
(load "humanjokerenplayer.ss")
(load "simpleguimanager.ss")

(define (GameManager GRules PlayerNames PlayerClasses DrawerClass)
  
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

(load "pestengamerules.ss")
(load "jokerengamerules.ss")

(define b '())
(define a '())
(if (eq? (read) 'pesten)
    (begin (set! b (PestenGameRules))
           (set! a (GameManager b '(Ruben Sander Jackie) (list HumanPlayer HumanPlayer HumanPlayer) SimpleGUIManager)))
    (begin (set! b (JokerenGameRules))
           (set! a (GameManager b '(Ruben Sander Jackie) (list HumanJokerenPlayer HumanJokerenPlayer HumanJokerenPlayer) SimpleGUIManager))))
(b 'RunRules)

