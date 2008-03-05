(load "simpleguiguidescriptor.ss")
(load "simpleguitableio.ss")
(load "simpleguiplayerio.ss")

(define (SimpleGUIManager GRules ThisPlayer)
  (define vpName "Card Game")
  (define width 1024)
  (define height 768)
  (open-graphics)
  
  (let* ((portDisplay (open-viewport vpName width height))
         (portMemory (open-pixmap (string-append vpName "OffScreen") width height))
         (guidesc (SimpleGUIGUIDescriptor portDisplay
                                          portMemory
                                          portMemory
                                          width
                                          height
                                          300
                                          250
                                          91
                                          106
                                          933
                                          662
                                          71
                                          96
                                          80
                                          105
                                          14
                                          20))
         (tableio (SimpleGUITableIO (GRules 'GetTable)
                                    guidesc))
         (playerio (SimpleGUIPlayerIO GRules
                                      ThisPlayer
                                      guidesc)))
    
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
    
    (define (WaitForMouse)
      (let ((mclick (get-mouse-click (guidesc 'ViewPortWindow))))
        (mouse-click-posn mclick)))
    
    (λ msg
      (if (null? msg)
          (error 'SimpleGUIManager "object requires a message")
          (case (car msg)
            ('TableChanged (TableChanged))
            ('DisplayUpdate (DisplayUpdate))
            ('Init (Init))
            ('GetTableSelect (tableio 'GetClick (WaitForMouse)))
            ('GetPlayerOwnCardsSelect (playerio 'GetOwnClick (WaitForMouse)))
            ('GetSelect (let ((c-posn (WaitForMouse)))
                          (or (tableio 'GetClick c-posn)
                              (playerio 'GetOwnClick c-posn))))
            ('Implements? (Implements? (GetParam msg 0)))
            (else (error 'SimpleGUIManager "message not understood: ~S" (car msg))))))))