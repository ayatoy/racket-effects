#lang racket

(require "../effects.rkt")

(define (absurd x) (error x))

(define (forall pred xs)
  (let loop ([xs xs])
    (cond [(null? xs) #t]
          [(pred (car xs)) (loop (cdr xs))]
          [else #f])))

;;
;; effect Fail : empty
;; effect Decide : bool
;;
(struct fail-effect ())
(struct decide-effect ())

;;
;; let fail () = absurd (perform Fail)
;; let decide () = perform Decide
;;
(define (fail x) (absurd (perform (fail-effect))))
(define (decide x) (perform (decide-effect)))

;;
;; let choose_true = handler
;;   | effect Decide k -> continue k true
;;
(define-handler choose-true
  [effect e k [(decide-effect? e) (k #t)]])

;;
;; let choose_max = handler
;;   | effect Decide k -> max (continue k true) (continue k false)
;;
(define-handler choose-max
  [effect e k [(decide-effect? e) (max (k #t) (k #f))]])

;;
;; let choose_all = handler
;;   | x -> [x]
;;   | effect Fail _ -> []
;;   | effect Decide k -> (continue k true) @ (continue k false)
;;
(define-handler choose-all
  [value x (list x)]
  [effect e k
    [(fail-effect? e) '()]
    [(decide-effect? e) (append (k #t) (k #f))]])

;;
;; with choose_true handle
;;   let x = (if decide () then 10 else 20) in
;;   let y = (if decide () then 0 else 5) in
;;   x - y
;;
(handle-with choose-true
  (let ([x (if (decide null) 10 20)]
        [y (if (decide null) 0 5)])
    (- x y)))

;;
;; with choose_max handle
;;   let x = (if decide () then 10 else 20) in
;;   let y = (if decide () then 0 else 5) in
;;   x - y
;;
(handle-with choose-max
  (let ([x (if (decide null) 10 20)]
        [y (if (decide null) 0 5)])
    (- x y)))

;;
;; with choose_all handle
;;   let x = (if decide () then 10 else 20) in
;;   let y = (if decide () then 0 else 5) in
;;   x - y
;;
(handle-with choose-all
  (let ([x (if (decide null) 10 20)]
        [y (if (decide null) 0 5)])
    (- x y)))

;;
;; let rec choose_int m n =
;;   if m > n then
;;     fail ()
;;   else if decide () then
;;     m
;;   else
;;     choose_int (m + 1) n
;;
(define (choose-int m n)
  (cond [(> m n) (fail null)]
        [(decide null) m]
        [else (choose-int (+ m 1) n)]))

;;
;; let int_sqrt m =
;;   let rec try n =
;;     let n2 = n ** 2 in
;;     if n2 > m then
;;       None
;;     else if n2 = m then
;;       Some n
;;     else
;;       try (n + 1)
;;   in
;;   try 0
;;
(define (int-sqrt m)
  (let try ([n 0])
    (let ([n2 (expt n 2)])
      (cond [(> n2 m) null]
            [(= n2 m) n]
            [else (try (+ n 1))]))))

;;
;; let pythagorean m n =
;;   let a = choose_int m (n - 1) in
;;   let b = choose_int a n in
;;   match int_sqrt (a ** 2 + b ** 2) with
;;   | None -> fail ()
;;   | Some c -> (a, b, c)
;;
(define (pythagorean m n)
  (let* ([a (choose-int m (- n 1))]
         [b (choose-int a n)]
         [c (int-sqrt (+ (expt a 2) (expt b 2)))])
    (if (null? c)
        (fail null)
        (vector a b c))))

;;
;; let backtrack = handler
;;   | effect Decide k ->
;;     handle continue k false with
;;     | effect Fail _ -> continue k true
;;
(define-handler backtrack
  [effect e k
    [(decide-effect? e)
     (handle ([effect e _ [(fail-effect? e) (k #t)]])
       (k #f))]])

;;
;; with backtrack handle
;;   pythagorean 5 15
;;
(handle-with backtrack
  (pythagorean 5 15))

;;
;; with choose_all handle
;;   pythagorean 3 4
;;
(handle-with choose-all
  (pythagorean 3 4))

;;
;; with choose_all handle
;;   pythagorean 5 15
;;
(handle-with choose-all
  (pythagorean 5 15))

;;
;; let rec choose = function
;;   | [] -> fail ()
;;   | x :: xs -> if decide () then x else choose xs
;;
(define (choose xs)
  (if (null? xs)
      (fail null)
      (if (decide null)
          (car xs)
          (choose (cdr xs)))))

;;
;; let no_attack (x, y) (x', y') =
;;   x <> x' && y <> y' && abs (x - x') <> abs (y - y')
;;
(define (no-attack v1 v2)
  (let ([x (vector-ref v1 0)] [y (vector-ref v1 1)]
        [v (vector-ref v2 0)] [w (vector-ref v2 1)])
    (and (not (= x v))
         (not (= y w))
         (not (= (abs (- x v))
                 (abs (- y w)))))))

;;
;; let available x qs =
;;   filter (fun y -> forall (no_attack (x, y)) qs) [1; 2; 3; 4; 5; 6; 7; 8]
;;
(define (available x qs)
  (filter (lambda (y) (forall (lambda (q) (no-attack (vector x y) q)) qs))
          (list 1 2 3 4 5 6 7 8)))

;;
;; let rec place x qs =
;;   if x = 9 then qs else
;;   let y = choose (available x qs) in
;;   place (x + 1) ((x, y) :: qs)
;;
(define (place x qs)
  (if (= x 9)
      qs
      (let ([y (choose (available x qs))])
        (place (+ x 1) (cons (vector x y) qs)))))

;;
;; with backtrack handle
;;   place 1 []
;;
(handle-with backtrack
  (place 1 '()))
