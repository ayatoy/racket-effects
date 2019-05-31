#lang racket

(require "../effects.rkt")

(struct print (message))

(define-handler default
  [effect e k
    [(print? e)
     (display (print-message e))
     (k null)]])

;;
;; perform (Print "Hello, world!\n")
;;
(handle-with default
  (perform (print "Hello, world!\n")))

;;
;; handle
;;   perform (Print "A");
;;   perform (Print "B");
;;   perform (Print "C");
;;   perform (Print "D")
;; with
;;   | effect (Print msg) k ->
;;     perform (Print ("I see you tried to print " ^ msg ^ ". Not so fast!\n"))
;;
(handle-with default
  (handle ([effect e k
             [(print? e)
              (perform (print (string-append
                               "I see you tried to print "
                               (print-message e)
                               ". Not so fast!\n")))]])
    (perform (print "A"))
    (perform (print "B"))
    (perform (print "C"))
    (perform (print "D"))))

;;
;; handle
;;   perform (Print "A");
;;   perform (Print "B");
;;   perform (Print "C");
;;   perform (Print "D")
;; with
;;   | effect (Print msg) k ->
;;     perform (Print ("I see you tried to print " ^ msg ^ ". Okay, you may.\n"));
;;     continue k ()
;;
(handle-with default
  (handle ([effect e k
             [(print? e)
              (perform (print (string-append
                               "I see you tried to print "
                               (print-message e)
                               ". Not so fast!\n")))
              (k null)]])
    (perform (print "A"))
    (perform (print "B"))
    (perform (print "C"))
    (perform (print "D"))))

;;
;; let collect = handler
;;   | x -> (x, "")
;;   | effect (Print msg) k ->
;;     let (result, msgs) = continue k () in
;;       (result, msg ^ msgs)
;;
(define-handler collect
  [value x (vector x "")]
  [effect e k
    [(print? e)
     (let ([v (k null)])
       (vector (vector-ref v 0)
               (string-append (print-message e)
                              (vector-ref v 1))))]])

;;
;; with collect handle
;;   perform (Print "A");
;;   perform (Print "B");
;;   perform (Print "C");
;;   perform (Print "D")
;;
(handle-with collect
  (perform (print "A"))
  (perform (print "B"))
  (perform (print "C"))
  (perform (print "D")))
