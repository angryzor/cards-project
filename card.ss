; ADT Card
;=========================================================================
; Specifications
;-------------------------------------------------------------------------
; Card (Constructor): ( symbol number ( Card Card -> number ) -> Card )
; Card.Color: ( -> symbol )
; Card.Value: ( -> number )
; Card.=?: ( Card -> boolean )

(define-macro CARD_HIGHER 1)
(define-macro CARD_EQUAL 0)
(define-macro CARD_LOWER -1)

(define (Card color value ==?)
  (define (Color)
    color)
  (define (Value)
    value)
  (define (=? Other)
    (==? (Card color value ==?) Other))
  (define (Implements? ClassDef)
    (eq? ClassDef 'Card))
  (λ msg
    (if (null? msg)
        (begin (display (cons (Color) (Value))))
        (case (car msg)
          ('Color (Color))
          ('Value (Value))
          ('=? (=? (GetParam msg 0)))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'Card "message not understood: ~S" (car msg)))))))
