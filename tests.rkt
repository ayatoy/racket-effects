#lang racket

(require rackunit "effects.rkt")

(check-equal?
 (handle-with* ((make-handler
                  [value x (+ x (expt 10 1))]
                  [finally x (+ x (expt 10 2))]
                  [effect e k [(eq? e 'A) (k 1)]])
                (make-handler
                  [value x (+ x (expt 10 3))]
                  [finally x (+ x (expt 10 4))])
                (make-handler
                  [value x (+ x (expt 10 5))]
                  [finally x (+ x (expt 10 6))]))
   (perform 'A))
 (foldr (lambda (m n) (+ (expt 10 m) n)) 1 (range 1 7))
 "value and finally handlers chain")

(check-equal?
 (handle ([value x (+ x (expt 10 1))]
          [finally x (+ x (expt 10 2))]
          [effect e k
            [(eq? e 'A)
             (handle ([value x (+ x (expt 10 3))]
                      [finally x (+ x (expt 10 4))]
                      [effect e k [(eq? e 'B) (k 1)]])
               (k null))]])
   (perform 'A)
   (perform 'B))
 (foldr (lambda (m n) (+ (expt 10 m) n)) 1 (range 1 5))
 "handle in effect handler")
