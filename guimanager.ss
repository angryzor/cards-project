(load "guicarddrawer.ss")

(define (GUIManager vpName width height)
  (define portDisplay (open-viewport vpName width height))
  (define portMemory (open-pixmap width height))
  (define out (GUICardDrawer))
  (define in (GUIInput))
  
  (define mydraw-viewport (draw-viewport portMemory))
  
  ;**********************************************************$
  ; BeginPaint                                               $
  ; Clears the screen with a color and prepares for painting $
  ;**********************************************************$
  (define (BeginPaint color)
    (mydraw-viewport color))
  
  (define (EndPaint)
    (copy-viewport portMemory portDisplay))
  
  
  