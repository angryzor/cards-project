(define (OriginCardPair origin card)
  (λ msg
    (if (null? msg)
        (error 'SimpleGUIPlayerIO "object requires a message")
        (case (car msg)
          ('Origin origin)
          ('Card card)
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'OriginCardPair "message not understood: ~S" (car msg)))))))