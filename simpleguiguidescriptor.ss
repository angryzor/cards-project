(define (SimpleGUIGUIDescriptor vpWindow
                                vpMem
                                vpTDT
                                vpWidth
                                vpHeight
                                maxHSetWidth
                                maxVSetHeight
                                tableX
                                tableY
                                tableWidth
                                tableHeight
                                cardWidth
                                cardHeight
                                stackWidth
                                stackHeight
                                maxHSpace
                                maxVSpace)
  (λ msg
    (if (null? msg)
        (error 'SimpleGUIPlayerIO "object requires a message")
        (case (car msg)
          ('ViewPortWindow vpWindow)
          ('ViewPortMemory vpMem)
          ('ViewPortToDrawTo vpTDT)
          ('ViewPortWidth vpWidth)
          ('ViewPortHeight vpHeight)
          ('MaxSetEvolveWidth maxHSetWidth)
          ('MaxSetEvolveHeight maxVSetHeight)
          ('TableX tableX)
          ('TableY tableY)
          ('TableWidth tableWidth)
          ('TableHeight tableHeight)
          ('CardWidth cardWidth)
          ('CardHeight cardHeight)
          ('StackWidth stackWidth)
          ('StackHeight stackHeight)
          ('MaxHSpace maxHSpace)
          ('MaxVSpace maxVSpace)
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'SimpleGUIGUIDescriptor "message not understood: ~S" (car msg)))))))