(define (TableSelectedCard tableItem card)
  (λ msg
    (if (null? msg)
        (error 'SimpleGUIPlayerIO "object requires a message")
        (case (car msg)
          ('TableItem tableItem)
          ('Card card)
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'TableSelectedCard "message not understood: ~S" (car msg)))))))