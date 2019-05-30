#lang racket

(require racket/control)

(provide make-handler define-handler
         perform with-handler handle-with handle-with* handle)

(define *current-handler* (make-parameter #f))
(define *tag* (make-continuation-prompt-tag))
(struct handler (value effect finally))
(struct %effect (value handler continue))

(define-syntax handler-clause
  (syntax-rules (value effect finally else)
    [(_ (value value-id body ...))
     (cons 'value
           (lambda (value-id) body ...))]
    [(_ (effect value-id cont-id (test expr ...) ...))
     (cons 'effect
           (lambda (value-id cont-id handler)
             (call/cc
              (lambda (break)
                (cond [test (break (begin expr ...))] ...)
                (perform/pc value-id
                            (lambda (value)
                              ((handler-finally handler) (cont-id value)))
                            handler)))))]
    [(_ (finally value-id body ...))
     (cons 'finally
           (lambda (value-id) body ...))]))

(define-syntax handler-clauses
  (syntax-rules (value effect finally else)
    [(_) null]
    [(_ clause rest ...)
     (cons (handler-clause clause)
           (handler-clauses rest ...))]))

(define-syntax make-handler
  (syntax-rules (value effect finally else)
    [(_ clause ...)
     (let ([handlers (handler-clauses clause ...)]
           [find-handler
            (lambda (type handlers)
              (let ([handler (findf (lambda (h) (eq? type (car h)))
                                    handlers)])
                (and handler (cdr handler))))])
       (handler
        (or (find-handler 'value handlers) identity)
        (or (find-handler 'effect handlers) (lambda (x k h) (void)))
        (or (find-handler 'finally handlers) identity)))]))

(define-syntax define-handler
  (syntax-rules (value effect finally else)
    [(_ name clause ...)
     (define name (make-handler clause ...))]))

(define (perform/pc value k1 h1)
  (let ([h2 (*current-handler*)])
    (if (not h2)
        (error "uncaught effect" value)
        (control-at *tag* k2
          (%effect value
                   h2
                   (lambda (x)
                     (%with-handler
                      h2
                      (lambda ()
                        (if (not k1)
                            (k2 x)
                            (k2 ((handler-finally h1) (k1 x))))))))))))

(define (perform value)
  (perform/pc value #f #f))

(define (%with-handler handler thunk)
  (let ([result (prompt-at *tag*
                  (parameterize ([*current-handler* handler])
                    (thunk)))])
    (cond [(%effect? result)
           ((handler-effect handler)
            (%effect-value result)
            (%effect-continue result)
            handler)]
          [else ((handler-value handler) result)])))

(define (with-handler handler thunk)
  ((handler-finally handler) (%with-handler handler thunk)))

(define-syntax handle-with
  (syntax-rules ()
    [(_ handler body ...)
     (with-handler handler (lambda () body ...))]))

(define-syntax handle-with*
  (syntax-rules ()
    [(_ (handler) body ...)
     (handle-with handler body ...)]
    [(_ (handler rest ...) body ...)
     (handle-with handler
       (handle-with* (rest ...) body ...))]))

(define-syntax handle
  (syntax-rules (value effect finally else)
    [(_ (clause ...) body ...)
     (handle-with (make-handler clause ...) body ...)]))
