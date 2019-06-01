#lang racket

(require "../effects.rkt")

(struct random-real ())

(define-handler default
  [effect e k
    [(random-real? e) (k (random))]])

;;
;; effect Toss : float -> bool
;;
(struct toss (probability))

;;
;; let rec uniform = function
;;   | [x] -> x
;;   | x :: xs ->
;;     let n = length xs + 1 in
;;     let p = 1.0 /. float_of_int n in
;;     if perform (Toss p) then x else uniform xs
;;
(define (uniform lst)
  (if (null? (cdr lst))
      (car lst)
      (let* ([n (+ 1 (length (cdr lst)))]
             [p (/ 1.0 n)])
        (if (perform (toss p))
            (car lst)
            (uniform (cdr lst))))))

;;
;; let random_value = handler
;;   | v -> v
;;   | effect (Toss p) k ->
;;     let toss = perform (RandomFloat 1.) < p in
;;     continue k toss
;;
(define-handler random-value
  [value v v]
  [effect e k
    [(toss? e)
     (let ([t (< (perform (random-real))
                 (toss-probability e))])
       (k t))]])

;;
;; let expectation = handler
;;   | v -> v
;;   | effect (Toss p) k ->
;;     p *. (continue k true) +. (1.0 -. p) *. (continue k false)
;;
(define-handler expectation
  [value v v]
  [effect e k
    [(toss? e)
     (let ([p (toss-probability e)])
       (+ (* p (k #t))
          (* (- 1.0 p) (k #f))))]])

;;
;; with expectation handle
;;   let x = uniform [1.; 2.; 3.; 4.; 5.; 6.] in
;;   let y = uniform [1.; 2.; 3.; 4.; 5.; 6.] in
;;   x +. y
;;
(handle-with expectation
  (let ([x (uniform '(1 2 3 4 5 6))]
        [y (uniform '(1 2 3 4 5 6))])
    (+ x y)))

;;
;; let combine p dist1 dist2 =
;;   let scale p dist = map (fun (x, q) -> (x, p *. q)) dist in
;;   let rec add (x, p) = function
;;     | [] -> [(x, p)]
;;     | (y, q) :: dist ->
;;       if x = y then (x, p +. q) :: dist else (y, q) :: add (x, p) dist
;;   in
;;   let dist1 = scale p dist1 in
;;   let dist2 = scale (1.0 -. p) dist2 in
;;   fold_right add dist1 dist2
;;
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

;;
;; let distribution = handler
;;   | v -> [(v, 1.0)]
;;   | effect (Toss p) k -> combine p (continue k true) (continue k false)
;;
(define-handler distribution
  [value v (list (vector v 1.0))]
  [effect e k
    [(toss? e)
     (combine (toss-probability e)
              (k #t)
              (k #f))]])

;;
;; with distribution handle
;;   let x = uniform [1; 2; 3; 4; 5; 6] in
;;   let y = uniform [1; 2; 3; 4; 5; 6] in
;;   x + y
;;
(handle-with distribution
  (let ([x (uniform '(1 2 3 4 5 6))]
        [y (uniform '(1 2 3 4 5 6))])
    (+ x y)))
