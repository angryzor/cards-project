; ADT CardStack
;================================================
(load "stack.ss")

(define (CardStack)
  (define stck (Stack))
  ;*******************************************************
  ; function shuffle
  ; -> Shuffles the deck
  ;
  ; @params: /
  ; @return: #t
  ;*******************************************************
  (define (shuffle)
    (define (@popall!)
      (if (empty? stck)
          '()
          (cons (stck 'pop!) (@popall!))))
    (define (@pushall! vec ctr)
      (if (< ctr (vector-length vec))
          (stck 'push! (vector-ref ctr))))
    (define (@do_shuffle vec ctr)
      (if (= ctr (vector-length 
    (let ((allcards (list->vector (@popall!))))
  ;*****************************************************************
  ; function Implements?
  ;
  ; @params: ClassDef (symbol)
  ; @return: true if the ADT supports the functions for the 
  ;          type passed via ClassDef, false otherwise
  ;*****************************************************************
  (define (Implements? ClassDef)
    (or (eq? ClassDef 'CardStack) (stck 'Implements? ClassDef)))
  (λ msg
    (if (null? msg)
        (stck)
        (case (car msg)
          ('shuffle (shuffle))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (apply stck msg))))))