(define (GUICardDrawer vp)
  (define (DrawCard card x y)
    (define (GetCardString)
      (if (and (procedure? card)
               (card 'Implements? 'Card))
          (string-append "C:\\Program Files\\PLT\\collects\\games\\cards\\hicolor\\"
                         (case (card 'Color)
                           ('klaveren "0-")
                           ('ruiten "1-")
                           ('harten "2-")
                           ('schoppen "3-"))
                         (- (card 'Value) 1)
                         ".png")))
    