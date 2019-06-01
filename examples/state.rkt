#lang racket

(require "../effects.rkt")

;;
;; effect Get: int
;; effect Set: int -> unit
;;
(struct get ())
(struct set (value))

;;
;; let monad_state = handler
;;   | y -> (fun _ -> y)
;;   | effect Get k -> (fun s -> (continue k s) s)
;;   | effect (Set s') k -> (fun _ -> (continue k ()) s')
;;
(define-handler monad-state
  [value y (lambda (_) y)]
  [effect e k
    [(get? e) (lambda (s) ((k s) s))]
    [(set? e) (lambda (_) ((k null) (set-value e)))]])

;;
;; let f = with monad_state handle
;;   let x = perform Get in
;;   perform (Set (2 * x));
;;   perform Get + 10
;; in
;; f 30
;;
(let ((f (handle-with monad-state
           (let ((x (perform (get))))
             (perform (set (* 2 x)))
             (+ (perform (get)) 10)))))
  (f 30))

;;
;; let better_state initial = handler
;;   | y -> (fun s -> (y, s))
;;   | effect Get k -> (fun s -> (continue k s) s)
;;   | effect (Set s') k -> (fun _ -> (continue k ()) s')
;;   | finally f -> f initial
;;
(define (better-state initial)
  (make-handler
   [value y (lambda (s) (list y s))]
   [effect e k
     [(get? e) (lambda (s) ((k s) s))]
     [(set? e) (lambda (_) ((k null) (set-value e)))]]
   [finally f (f initial)]))

;;
;; with better_state 30 handle
;;   let x = perform Get in
;;   perform (Set (2 * x));
;;   perform Get + 10
;;
(handle-with (better-state 30)
  (let ((x (perform (get))))
    (perform (set (* 2 x)))
    (+ (perform (get)) 10)))
