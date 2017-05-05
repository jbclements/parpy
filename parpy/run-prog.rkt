#lang typed/racket

(provide python-executable
         run-py-file)
(define-predicate my-path-string? Path-String)

(define python-executable
  (make-parameter
   "/opt/local/bin/python"
   (λ (v)
     (cond [(my-path-string? v) v]
           [else (error 'python-executable
                        "expected path-string, got: ~e"
                        v)]))))


;; given the name of a python file, run it and report
;; all output on stdout and stderr.
(define (run-py-file [python-file : Path-String]) : Void
  (match-define (list p-stdout p-stdin pid p-stderr control-proc)
    (process* (python-executable) python-file))
  (observe-ports (list p-stdout p-stderr))
  (void))

;; given a set of ports, show any data that shows up on any of
;; them. Stops observing a port when it returns #<eof>.
;; Returns 'all-done if all ports report #<eof>
(define (observe-ports [ports : (Listof Input-Port)]) : Symbol
  (let loop : Symbol ([live-ports ports])
    (cond
      [(empty? live-ports)
       'all-done]
      [else
       (define port-with-output
         (apply sync live-ports))
       (displayln (object-name port-with-output))
       (match (read-bytes 1000 port-with-output)
         [(? eof-object? e)
          (displayln e)
          (loop (remq port-with-output live-ports))]
         [(? bytes? b)
          (displayln b)
          (loop live-ports)])])))