(define (SimpleGUIGUIDescriptor vpWindow
                                vpMem
                                vpWidth
                                vpHeight
                                maxHSetWidth
                                maxVSetHeight
                                tableX
                                tableY
                                tableWidth
                                tableHeight
                                tableEvolveDirection
                                cardWidth
                                cardHeight)
  (λ msg
    (if (null? msg)
        (error 'SimpleGUIPlayerIO "object requires a message")
        (case (car msg)
          ('ViewPortWindow vpWindow)
          ('ViewPortMemory vpWindow)
          ('ViewPortWidth vpWidth)
          ('ViewPortHeight vpHeight)
          ('MaxSetEvolveWidth maxHSetWidth)
          ('MaxSetEvolveHeight maxVSetHeight)
          ('TableX tableX)
          ('TableY tableY)
          ('TableWidth tableWidth)
          ('TableHeight tableHeight)
          ('TableEvolveDirection tableEvolveDirection)
          ('CardWidth cardWidth)
          ('CardHeight cardHeight)
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'SimpleGUIGUIDescriptor "message not understood: ~S" (car msg)))))))