(load "double-linked-position-list.ss")

(define (CardSet)
  (define (Card-ComparisonCaller x y)
    (x '=? y))
  (define plst (position-list Card-ComparisonCaller))
  
  (define (add! card)
    (if (and (procedure? card)
             (card 'Implements? 'Card))
        (plst 'add-after! card)
        (error 'CardSet.add! "expects type <Card> as argument, given: ~S" card)))
  
  (define (delete! card)
    (if (and (procedure? card)
             (card 'Implements? 'Card))
        (plst 'delete! (plst 'find card))
        (error 'CardSet.delete! "expects type <Card> as argument, given: ~S" card)))
  
  (define (getAllCards)
    (define (iter nowpos lst)
      (if (plst 'has-next? nowpos)
          (iter (plst 'next nowpos) (cons (plst 'value nowpos) lst))
          (reverse lst)))
    (if (not (plst 'empty?))
        (iter (plst 'first-position) '())))
  
  