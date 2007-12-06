(load "double-linked-position-list.ss")

(define (CardSet)
  (define (Card-ComparisonCaller x y)
    (x '=? y))
  (define plst (double-linked-position-list Card-ComparisonCaller))
  