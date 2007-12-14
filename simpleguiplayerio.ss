(load "simpleguicarddrawer.ss")

(define (SimpleGUIPlayerIO vp GRules pThisPlayer vpWidth vpHeight)
  (define positions '())
  (define maxVSetHeight 300)
  (define maxHSetWidth 310)
  (define cardWidth 71)
  (define cardHeight 96)
  (define ThisPlayer '())
  
  (define cd (SimpleGUICardDrawer vp))
  
  (define (PlacementDef pos evo)
    (define (getpos)
      pos)
    (define (getevo)
      evo)
    
    (λ msg
      (if (null? msg)
          (error 'PlacementDef "object requires a message")
          (case (car msg)
            ('pos (getpos))
            ('evo (getevo))
            ('Implements? (Implements? (GetParam msg 0)))
            (else (error 'PlacementDef "message not understood: ~S" (car msg)))))))
  
  (define (MapPosition x)
    (define (AdjustToMap i)
      (if (< (- i ThisPlayer) 0)
          (+ (GRules 'NumPlayers) (- i ThisPlayer))
          (- i ThisPlayer)))
    (case (AdjustToMap x)
      ((0) (PlacementDef (make-posn cardWidth (- vpHeight cardHeight)) 'horizontal))
      ((1) (PlacementDef (make-posn 0 (/ vpHeight 2)) 'vertical))
      ((2) (PlacementDef (make-posn 0 0) 'vertical))
      ((3) (PlacementDef (make-posn cardWidth 0) 'horizontal))
      ((4) (PlacementDef (make-posn (/ vpWidth 2) 0) 'horizontal))
      ((5) (PlacementDef (make-posn (- vpWidth cardWidth) 0) 'vertical))
      ((6) (PlacementDef (make-posn (- vpWidth cardWidth) (/ vpHeight 2)) 'vertical))
      ((7) (PlacementDef (make-posn (/ vpWidth 2) (- vpHeight cardHeight)) 'horizontal))
      (else (error 'SimpleGUIPlayerIO.MapPosition "Cannot place player with mapping ~S" x))))
  
  
  (define (Init)
    (set! positions (make-vector (GRules 'NumPlayers)))
    (set! ThisPlayer (GRules 'GetPlayerIndex pThisPlayer))
    (let ((nump (GRules 'NumPlayers)))
      (define (PlacePlayers i)
        (if (not (= i nump))
            (begin
              (vector-set! positions i (MapPosition i))
              (PlacePlayers (+ i 1)))))
      (PlacePlayers 0)))
  
  (define (Draw)
    (let ((nump (GRules 'NumPlayers)))
      (define (DrawPlayers i)
        (if (not (= i nump))
            (begin
              (cd 'DrawCardSet
                  ((GRules 'GetPlayer i) 'getHand)
                  (posn-x ((vector-ref positions i) 'pos))
                  (posn-y ((vector-ref positions i) 'pos))
                  (if (eq? ((vector-ref positions i) 'evo) 'horizontal)
                      maxHSetWidth
                      maxVSetHeight)
                  ((vector-ref positions i) 'evo))
              (DrawPlayers (+ i 1)))))
      (DrawPlayers 0)))
  
  (define (GetClickOwnCard)
    ())
  
  (λ msg
    (if (null? msg)
        (error 'SimpleGUIPlayerIO "object requires a message")
        (case (car msg)
          ('Init (Init))
          ('Draw (Draw))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'SimpleGUIPlayerIO "message not understood: ~S" (car msg)))))))