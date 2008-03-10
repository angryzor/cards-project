; ADT CardStack
;================================================
; Het Knuth Shuffle algoritme is ontworpen door Mark Knuth. (gevonden op wikipedia)
; De implementatie ervan is mijn eigen werk
;
(load "stack.ss")
(require (lib "trace.ss"))

(define (CardStack . up??)
  (define stck (Stack))
  (define up? (if (null? up??)
                  #t
                  (car up??)))
  ;*******************************************************
  ; function shuffle
  ; -> Shuffles the deck
  ;
  ; @params: /
  ; @return: #t
  ;*******************************************************
  (define (shuffle)
    (define (popall!)
      (if (stck 'empty?)
          '()
          (cons (stck 'pop!) (popall!))))
    (define (pushall! vec ctr)
      (if (< ctr (vector-length vec))
          (begin (stck 'push! (vector-ref vec ctr))
                 (pushall! vec (+ ctr 1)))))
    (define (knuth_shuffle vec)
      (define (knuth_shuffle_inner vec n)
        (if (< n 1)
            vec
            (let ((k (random (+ n 1))))
              (if (not (= k n))
                  (let ((tmp (vector-ref vec k)))
                    (vector-set! vec k (vector-ref vec n))
                    (vector-set! vec n tmp)))
              (knuth_shuffle_inner vec (- n 1)))))
      (knuth_shuffle_inner vec (- (vector-length vec) 1)))
    (pushall!
     (knuth_shuffle
      (list->vector
       (popall!))) 0))

  (define (flipDown)
    (set! up? #f))
  
  (define (flipUp)
    (set! up? #t))
  
  (define (GUIDrawCall x y cd)
    (cd 'DrawCardStack
                dispatch
                x
                y))
  
  (define (GUIWidth desc)
    (desc 'StackWidth))
  
  (define (GUIEvoHeight desc)
    (desc 'StackHeight))
  
;*****************************************************************
  ; function Implements?
  ;
  ; @params: ClassDef (symbol)
  ; @return: true if the ADT supports the functions for the 
  ;          type passed via ClassDef, false otherwise
  ;*****************************************************************
  (define (Implements? ClassDef)
    (or (eq? ClassDef 'CardStack) (stck 'Implements? ClassDef)))
  (define (dispatch . msg)
    (if (null? msg)
        (stck)
        (case (car msg)
          ('shuffle (shuffle))
          ('flipDown (flipDown))
          ('flipUp (flipUp))
          ('faceUp? up?)
          ('GUIDrawCall (GUIDrawCall (GetParam msg 0) (GetParam msg 1) (GetParam msg 2)))
          ('GUIWidth (GUIWidth (GetParam msg 0)))
          ('GUIHeight (GUIEvoHeight (GetParam msg 0)))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (apply stck msg)))))
  dispatch)


