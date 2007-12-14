(load "simpleguicarddrawer.ss")

(define (SimpleGUITableIO vp table bb-x bb-y bb-w bb-h)
  (define positions (position-list (λ (x y) #f)))
  (define maxVSetHeight 300)
  (define maxHSetWidth 350)
  (define cardWidth 71)
  (define cardHeight 96)
  (define stackWidth 79)
  
  (define cd (SimpleGUICardDrawer vp))
  
  (define (Recalculate)
    (let ((poslst (table 'toPosList)))
      (define (NextPos pos x y)
        (define cardWidth (if (ObjectOfType? 'CardStack (poslst 'value pos))
                              stackWidth
                              cardWidth))
        (set! x (+ x cardWidth))
        (if (> x (- bb-w cardWidth))
            (begin
              (set! x 0)
              (set! y (+ y maxVSetHeight))))
        (values x y))
      
      (define (iter pos x y)
        (positions 'add-after! (make-posn (+ x bb-x) (+ y bb-y)))
        (if (poslst 'has-next? pos)
            (call-with-values (λ () (NextPos pos x y))
                              (λ (newx newy) (iter (poslst 'next pos) newx newy)))))
      (if (not (poslst 'empty?))
          (iter (poslst 'first-position) 0 0))))
  
  (define (GetClick)
    '())
  
  (define (Draw)
    (let ((poslst (table 'toPosList)))
      (define (iter pos pospos)
        (if (ObjectOfType? 'CardStack (poslst 'value pos))
            (cd 'DrawCardStack 
                (poslst 'value pos) 
                (posn-x (positions 'value pospos))
                (posn-y (positions 'value pospos)))
            (cd 'DrawCardSet
                (poslst 'value pos) 
                (posn-x (positions 'value pospos))
                (posn-y (positions 'value pospos))
                maxVSetHeight
                'vertical))
        (if (poslst 'has-next? pos)
            (iter (poslst 'next pos) (positions 'next pospos))))
      (if (not (poslst 'empty?))
          (if (= (poslst 'length) (positions 'length))
              (iter (poslst 'first-position) (positions 'first-position))
              (error 'SimpleGUITableIO.Draw "positions do not appear to have been refreshed")))))
  
  (λ msg
    (if (null? msg)
        (error 'SimpleGUITableIO "object requires a message")
        (case (car msg)
          ('Recalculate (Recalculate))
          ('Draw (Draw))
          ('Implements? (Implements? (GetParam msg 0)))
          (else (error 'SimpleGUITableIO "message not understood: ~S" (car msg)))))))