#lang racket

(require "../effects.rkt")

(struct print (message))

(define-handler default
  [effect e k
    [(print? e)
     (display (print-message e))
     (k null)]])

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

(define-handler collect
  [value x (vector x "")]
  [effect e k
    [(print? e)
     (let ([v (k null)])
       (vector (vector-ref v 0)
               (string-append (print-message e)
                              (vector-ref v 1))))]])

(handle-with collect
  (perform (print "A"))
  (perform (print "B"))
  (perform (print "C"))
  (perform (print "D")))
