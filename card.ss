; ADT Card
;=========================================================================
; Specifications
;-------------------------------------------------------------------------
; Card (Constructor): ( symbol number ( Card Card -> number ) -> Card )
; Card.Color: ( -> symbol )
; Card.Value: ( -> number )
; Card.=?: ( Card -> boolean )
; Card.<?: ( Card -> boolean )
; Card.>?: ( Card -> boolean )
; Card.<=?: ( Card -> boolean )
; Card.>=?: ( Card -> boolean )
; Card.Implements?: ( symbol -> boolean )

;Values that may be used by the comparison function passed to Card.
(define Card.CARD_HIGHER 1)
(define Card.CARD_EQUAL 0)
(define Card.CARD_LOWER -1)


(define (Card color value ==?)
  ;*****************************************************************
  ; function Color
  ;
  ; @params: /
  ; @return: color of the card
  ;*****************************************************************
  (define (Color)
    color)
  ;*****************************************************************
  ; function Value
  ;
  ; @params: /
  ; @return: value of the card
  ;*****************************************************************
  (define (Value)
    value)
  ;*****************************************************************
  ; function =?
  ;
  ; @params: Other (Card)
  ; @return: true if this card is equal to Other, false otherwise
  ;*****************************************************************
  (define (=? Other)
    (= (==? (Card color value ==?) Other) 0))
  ;*****************************************************************
  ; function <?
  ;
  ; @params: Other (Card)
  ; @return: true if this card is lower than Other, false
  ;          otherwise
  ;*****************************************************************
  (define (<? Other)
    (= (==? (Card color value ==?) Other) -1))
  ;*****************************************************************
  ; function >?
  ;
  ; @params: Other (Card)
  ; @return: true if this card is higher than Other, false
  ;          otherwise
  ;*****************************************************************
  (define (>? Other)
    (= (==? (Card color value ==?) Other) 1))
  ;*****************************************************************
  ; function <=?
  ;
  ; @params: Other (Card)
  ; @return: true if this card is lower than or equal to Other,
  ;          false otherwise
  ;*****************************************************************
  (define (<=? Other)
    (let ((cmpres (==? (Card color value ==?) Other)))
      (or (= cmpres -1) (= cmpres 0))))
  ;*****************************************************************
  ; function >=?
  ;
  ; @params: Other (Card)
  ; @return: true if this card is higher than or equal to Other,
  ;          false otherwise
  ;*****************************************************************
  (define (>=? Other)
    (let ((cmpres (==? (Card color value ==?) Other)))
      (or (= cmpres 0) (= cmpres 1))))
  
  (define (my-equal? Other)
    (and (eq? color (Other 'Color))
         (= value (Other 'Value))))
  ;*****************************************************************
  ; function Implements?
  ;
  ; @params: ClassDef (symbol)
  ; @return: true if the ADT supports the functions for the 
  ;          type passed via ClassDef, false otherwise
  ;*****************************************************************
  (define (Implements? ClassDef)
    (eq? ClassDef 'Card))
  
  ;Check for wrong types of arguments
  (if (not (symbol? color))
      (error 'Card "expects type <symbol> as 1st argument, given: ~S" color))
  (if (not (number? value))
      (error 'Card "expects type <number> as 1st argument, given: ~S" value))
  
  (λ msg
    (if (null? msg)
        (begin (display (cons (Color) (Value))))
        (case (car msg)
          ('Color (Color))
          ('Value (Value))
          ('=? (=? (GetParam msg 0)))
          ('<? (<? (GetParam msg 0)))
          ('>? (>? (GetParam msg 0)))
          ('<=? (<=? (GetParam msg 0)))
          ('>=? (>=? (GetParam msg 0)))
          ('equal? (my-equal? (GetParam msg 0)))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'Card "message not understood: ~S" (car msg)))))))
