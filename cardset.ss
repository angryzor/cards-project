(load "double-linked-position-list.ss")

(define (CardSet)
  (define (Card-ComparisonCaller x y)
    (x '=? y))
  (define plst (double-linked-position-list Card-ComparisonCaller))
  
  (define (add! card)
    (if (and (procedure? card)
             (card 'Implements? 'Card))
        (plst 'add-after! card)
        (error 'CardSet.add! "expects type <Card> as argument, given: ~S" card))))