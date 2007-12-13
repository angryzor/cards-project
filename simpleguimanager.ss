(load "simpleguicarddrawer.ss")

(define (SimpleGUIManager vpName width height)
  (define portDisplay (open-viewport vpName width height))
  (define portMemory (open-pixmap width height))
  (define out (SimpleGUICardDrawer portMemory))
  (define in (SimpleGUIInput))
  
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
  
  (define (Implements? ClassDef)
    (eq? ClassDef 'SimpleGUIManager))
  
  (λ msg
    (if (null? msg)
        (top)
        (case (car msg)
          ('BeginPaint (BeginPaint (GetParam msg 0)))
          ('EndPaint (EndPaint))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'SimpleGUIManager "message not understood: ~S" (car msg)))))))