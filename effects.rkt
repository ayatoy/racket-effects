#lang racket

(require racket/control)

(provide make-handler define-handler
         perform with-handler handle-with handle)

(struct handler (value effect finally))

(define-syntax handler-clause
  (syntax-rules (value effect finally else)
    [(_ (value x body ...))
     (cons 'value
           (lambda (x) body ...))]
    [(_ (effect x k (test expr ...) ...))
     (cons 'effect
           (lambda (x k break)
             (cond (test (break (begin expr ...))) ...)))]
    [(_ (finally x body ...))
     (cons 'finally
           (lambda (x) body ...))]))

(define-syntax handler-clauses
  (syntax-rules (value effect finally else)
    [(_) null]
    [(_ clause rest ...)
     (cons (handler-clause clause)
           (handler-clauses rest ...))]))

(define-syntax make-handler
  (syntax-rules (value effect finally else)
    [(_ clause ...)
     (let ((handlers (handler-clauses clause ...))
           (find-handler
            (lambda (type handlers)
              (let ((handler (findf (lambda (h) (eq? type (car h)))
                                    handlers)))
                (and handler (cdr handler))))))
       (handler
        (or (find-handler 'value handlers) identity)
        (or (find-handler 'effect handlers) (lambda (x k break) (void)))
        (or (find-handler 'finally handlers) identity)))]))

(define-syntax define-handler
  (syntax-rules (value effect finally else)
    [(_ name clause ...)
     (define name (make-handler clause ...))]))

(define *effect-handler-procs* (make-parameter '()))

(define (perform value)
  (let ((eh-procs (*effect-handler-procs*)))
    (if (null? eh-procs)
        (error "uncaught effect" value)
        (shift k (call/cc
                  (lambda (break)
                    (let loop ((eh-procs eh-procs))
                      (if (null? eh-procs)
                          (error "uncaught effect" value)
                          (let ((eh-proc (car eh-procs))
                                (rest-procs (cdr eh-procs)))
                            (parameterize ((*effect-handler-procs* rest-procs))
                              (eh-proc value
                                       (lambda args (reset (apply k args)))
                                       break))
                            (loop rest-procs))))))))))

(define (with-handler handler thunk)
  ((handler-finally handler)
   (reset
    (parameterize
        ((*effect-handler-procs*
          (cons (handler-effect handler)
                (*effect-handler-procs*))))
      ((handler-value handler)
       (thunk))))))

(define-syntax handle-with
  (syntax-rules ()
    [(_ handler body ...)
     (with-handler handler (lambda () body ...))]))

(define-syntax handle
  (syntax-rules (value effect finally else)
    [(_ (clause ...) body ...)
     (handle-with (make-handler clause ...) body...)]))
