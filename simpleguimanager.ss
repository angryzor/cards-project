(load "simpleguitableio.ss")
(load "simpleguiplayerio.ss")

(define (SimpleGUIManager GRules ThisPlayer)
  (define vpName "Card Game")
  (define width 1024)
  (define height 768)
  (define cardWidth 71)
  (define cardHeight 96)
  (open-graphics)
  
  (let* ((portDisplay (open-viewport vpName width height))
         (portMemory (open-pixmap (string-append vpName "OffScreen") width height))
         (tableio (SimpleGUITableIO portMemory
                                    (GRules 'GetTable)
                                    (+ cardWidth 20)
                                    (+ cardHeight 10)
                                    (- width cardWidth 20)
                                    (- height cardHeight 10)))
         (playerio (SimpleGUIPlayerIO portMemory
                                      GRules
                                      ThisPlayer
                                      width
                                      height)))
;    (define in (SimpleGUIInput))
    
    ;***********************************************************
    ; Viewport operations shortened                            *
    ;***********************************************************
    (define mydraw-viewport (draw-viewport portMemory))
    
    ;**********************************************************$
    ; BeginPaint                                               $
    ; Clears the screen with a color and prepares for painting $
    ;**********************************************************$
    (define (BeginPaint color)
      (mydraw-viewport color))
    
    (define (EndPaint)
      (copy-viewport portMemory portDisplay))
    
    (define (TableChanged)
      (tableio 'Recalculate))
    
    (define (DisplayUpdate)
      (BeginPaint (make-rgb 0 0.5 0))
      (tableio 'Draw)
      (playerio 'Draw)
      (EndPaint))
    
    (define (Implements? ClassDef)
      (eq? ClassDef 'SimpleGUIManager))
    
    (define (Init)
      (playerio 'Init))
    
    (λ msg
      (if (null? msg)
          (error 'SimpleGUIManager "object requires a message")
          (case (car msg)
            ('TableChanged (TableChanged))
            ('DisplayUpdate (DisplayUpdate))
            ('Init (Init))
            ('Implements? (Implements? (GetParam msg 0)))
            (else (error 'SimpleGUIManager "message not understood: ~S" (car msg))))))))