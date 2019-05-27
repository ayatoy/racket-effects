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
           (lambda (x break tag ctx)
             (cond (test (break tag ctx (lambda (k) expr ...))) ...)))]
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
        (or (find-handler 'effect handlers) (lambda (x break tag ctx) (void)))
        (or (find-handler 'finally handlers) identity)))]))

(define-syntax define-handler
  (syntax-rules (value effect finally else)
    [(_ name clause ...)
     (define name (make-handler clause ...))]))

(define *context* (make-parameter null))

(define (perform value)
  (let ([context (*context*)])
    (if (null? context)
        (error "uncaught effect" value)
        (let-values
            ([(tag context ehp)
              (call/cc
               (lambda (break)
                 (let loop ([context context])
                   (if (null? context)
                       (error "uncaught effect" value)
                       (let ([tag (caar context)]
                             [find-ehp (cdar context)]
                             [rest (cdr context)])
                         (find-ehp value break tag rest)
                         (loop rest))))))])
          (parameterize ([*context* context])
            (shift-at tag k (ehp k)))))))

(define (with-handler handler thunk)
  (let ((tag (make-continuation-prompt-tag)))
    ((handler-finally handler)
     (reset-at tag
       (parameterize
           ((*context*
             (cons (cons tag (handler-effect handler))
                   (*context*))))
         ((handler-value handler)
          (thunk)))))))

(define-syntax handle-with
  (syntax-rules ()
    [(_ handler body ...)
     (with-handler handler (lambda () body ...))]))

(define-syntax handle
  (syntax-rules (value effect finally else)
    [(_ (clause ...) body ...)
     (handle-with (make-handler clause ...) body...)]))
