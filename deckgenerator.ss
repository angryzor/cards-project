(define (DeckGenerator cardcmp)
  (define curVal 1)
  (define curCol 'schoppen)
  
  (define (nextColor)
    (case curCol
      ('schoppen 'ruiten)
      ('ruiten 'klaveren)
      ('klaveren 'harten)
      (else 'schoppen)))
  
  (define (nextCard)
    (let ((tcv curVal)
          (tcc curCol))
      (set! curVal (+ curVal 1))
      (if (> curVal 13)
          (begin (set! curCol (nextColor))
                 (set! curVal 1)))
      (Card tcc tcv cardcmp)))
  
  (define (joker)
    (Card 'joker 0 cardcmp))
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'DeckGenerator))
  
  (λ msg
    (if (null? msg)
        (top)
        (case (car msg)
          ('NextCard (nextCard))
          ('Joker (joker))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'DeckGenerator "message not understood: ~S" (car msg)))))))