#lang racket

(require "../effects.rkt")

(struct random-real ())
(struct toss (probability))

(define (uniform lst)
  (if (null? (cdr lst))
      (car lst)
      (let* ([n (+ 1 (length (cdr lst)))]
             [p (/ 1.0 n)])
        (if (perform (toss p))
            (car lst)
            (uniform (cdr lst))))))

(define-handler default
  [effect e k
    [(random-real? e) (k (random))]])

(define-handler random-value
  [value v v]
  [effect e k
    [(toss? e)
     (let ([t (< (perform (random-real))
                 (toss-probability e))])
       (k t))]])

(define-handler expectation
  [value v v]
  [effect e k
    [(toss? e)
     (let ([p (toss-probability e)])
       (+ (* p (k #t))
          (* (- 1.0 p) (k #f))))]])

(handle-with expectation
  (let ([x (uniform '(1 2 3 4 5 6))]
        [y (uniform '(1 2 3 4 5 6))])
    (+ x y)))

(define (combine p dist1 dist2)
  (let* ([scale (lambda (p dist)
                  (map (lambda (v)
                         (vector (vector-ref v 0)
                                 (* p (vector-ref v 1))))
                       dist))]
         [dist1 (scale p dist1)]
         [dist2 (scale (- 1.0 p) dist2)])
    (letrec ([add (lambda (v vs)
                    (if (null? vs)
                        (list v)
                        (let* ([w (car vs)] [dist (cdr vs)]
                               [x (vector-ref v 0)] [p (vector-ref v 1)]
                               [y (vector-ref w 0)] [q (vector-ref w 1)])
                          (if (= x y)
                              (cons (vector x (+ p q))
                                    dist)
                              (cons (vector y q)
                                    (add v dist))))))])
      (foldr add dist2 dist1))))

(define-handler distribution
  [value v (list (vector v 1.0))]
  [effect e k
    [(toss? e)
     (combine (toss-probability e)
              (k #t)
              (k #f))]])

(handle-with distribution
  (let ([x (uniform '(1 2 3 4 5 6))]
        [y (uniform '(1 2 3 4 5 6))])
    (+ x y)))
