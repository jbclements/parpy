#lang racket

(module lambda-calculus racket
    (provide
     #%module-begin
     (rename-out [1-arg-lambda lambda]
                         [1-arg-app #%app]
                         ;[1-form-module-begin #%module-begin]
                         [no-literals #%datum]
                         [unbound-as-quoted #%top]))

  
  (define-syntax-rule (1-arg-lambda (x) expr)
    (lambda (x) expr))

  
  (define-syntax (1-arg-app stx)
    (syntax-case stx ()
      [(_ e1 e2)
       #'(begin (#%app printf "abcd\n")
                (#%app e1 e2))]))
  #;(define-syntax-rule (1-arg-app e1 e2)
    (begin (#%app printf "abcd\n")
           (#%app e1 e2)))
  
  
  (define-syntax (no-literals stx)
    (syntax-case stx ()
      [(_ . n)
       (cond
         [(number? (syntax-e #'n))
          #`(#%datum . #,(number->string (syntax-e #'n)))]
         [else
          #`(#%datum . "<unknown>")])]))

  (define-syntax-rule (unbound-as-quoted . id)
    'id))

(module ok (submod ".." lambda-calculus)
  1234
  zz)
(require 'ok)
