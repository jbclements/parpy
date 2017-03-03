#lang typed/racket

;; too early to decide what should be exported
(provide (all-defined-out))

;; represents a list of python lines
(define-type Block (Listof String))
(define-predicate block? Block)

;; given a name and a list of blocks, return a block
;; representing the test class
(define (testclass [name : Symbol] [blocks : (Listof Block)]) : Block
  (block-indent
   (cons (~a "class "name"(unittest.TestCase):")
         (flattenblocks blocks))))

;; given a name and a set of test sexps, generate
;; python tests named test<name>0, test<name>1, etc.
(: t (Symbol Sexp * -> Block))
(define (t name . sexps) : Block
  (id-check name)
  (flattenblocks
   (for/list : (Listof Block)
     ([str (in-list sexps)]
      [i (in-naturals)])
     (t1 (~a name i) str))))

;; format a test case string, as it would appear in
;; a class extending TestCase:
(: t1 (String Sexp * -> Block))
(define (t1 name . exps)
  (py-def (list (string->symbol (~a "test" name)) 'self)
          exps))

;; given a nonempty block, indent all strings after the first by 4
(define (block-indent [strs : Block]) : Block
  (cons (first strs)
        (map (indentstr 4) (rest strs))))

;; given a number and a string, indent the string by the number of chars
(define ((indentstr [n : Natural]) [str : String]) : String
  (string-append (apply string (for/list : (Listof Char) ([i n]) #\space)) str))

;; ... looks like this is becoming some kind of flatten-statement?
;; flatten an s-exp, put it in a list, but allow splicing
(define (py-flatten-l [exp : Sexp]) : Block
  (match exp
    [(cons 'block (? block? b)) b]
    [(list 'for (? symbol? loopvar) range bodys ...)
     (block-indent
      (cons (~a "for "loopvar" in "(py-flatten range)":")
            (apply append (map py-flatten-l bodys))))]
    [(list 'if test (list thens ...))
     (block-indent (cons (~a "if "(py-flatten test)":")
                         (apply append (map py-flatten-l thens))))]
    [other (list (py-flatten other))]))

;; convert an s-expression to a python string,
;; e.g. '(a b c) into "a(b, c)"
(define (py-flatten [a : Sexp]) : String
  (match a
    [(list '== a b)
     (infixop "==" a b)]
    [(list '= a b)
     (~a (py-flatten a) " = " (py-flatten b))]
    [(list '< a b)
     (infixop "<" a b)]
    [(list '% a b)
     (infixop "%" a b)]
    [(list 'not a)
     (paren-wrap (space-append "not" (py-flatten a)))]
    [(list 'asub arr idx)
     (string-append
      (py-flatten arr)
      (bracket-wrap (py-flatten idx)))]
    [(list 'return a)
     (space-append "return" (py-flatten a))]
    [(list 'noquote (? string? s)) s]
    [(cons 'arr (? list? args))
     (pylist (map py-flatten args))]
    [(list (? symbol? fn) args ...)
     (define funname (pyfunname fn))
     (id-check funname)
     (~a funname (arglist (map py-flatten args)))]
    [(? string? s) (string-append "\"" s "\"")]
    [(? symbol? s)
     (id-check s)
     (symbol->string s)]
    [other (~a a)]))

;; signal an error if the given identifier is not a legal python identifier
(define (id-check [s : Symbol]) : Void
  (unless (regexp-match? #px"^[.a-zA-Z0-9_]+$" (symbol->string s))
    (raise-argument-error 'id-check
                          "symbol legal as python identifier"
                          0 s)))

;; a mutation check. Ensure that after evaluating the expression,
;; the original value has been mutated to the later one.
(define (assert-mut [name : Symbol] [initval : Sexp]
                    [call : Sexp] [callresult : Sexp]
                    [expected : Sexp]) : (Pairof 'block Block)
  (exps-block `((= ,name ,initval)
                (assertEqual ,call ,callresult)
                (assertEqual ,name ,expected))))

;; a general mutation check. Ensure that after evaluating the expression,
;; the original value has been mutated to the later one.
(define (exps-block [exps : (Listof Sexp)]) : (Pairof 'block Block)
  (cons 'block (map py-flatten exps)))

;; append with spaces between
(: space-append (String * -> String))
(define (space-append . strs)
  (apply string-append (add-between strs " ")))

(check-equal? (space-append "a" "b" "c") "a b c")

;; wrap with ()
(define (paren-wrap [str : String]) : String
  (string-append "(" str ")"))

;; wrap with []
(define (bracket-wrap [str : String]) : String
  (string-append "[" str "]"))

;; insert operator with strings, wrap with parens
(define (infixop [op : String] [arg1 : Sexp] [arg2 : Sexp])
  (paren-wrap (space-append (py-flatten arg1) op (py-flatten arg2))))

(check-equal? (infixop "==" 'abc "def") "(abc == \"def\")")

;; concat strings with , between
(define (commasep [strs : (Listof String)]) : String
  (apply string-append (add-between strs ", ")))

;; concat strings with , between and parens outside
;; e.g.: "a" "b" -> "(a, b)""
(define (arglist [strs : (Listof String)]) : String
  (paren-wrap (commasep strs)))

(check-equal? (arglist '("a" "b")) "(a, b)")

;; concat strings with , between and [] outside
;; e.g.: "a" "b" => "[a, b]"
(define (pylist [strs : (Listof String)]) : String
  (bracket-wrap (commasep strs)))

(check-equal? (pylist '("a" "b")) "[a, b]")

;; convert various symbols to other strings
(define (pyfunname [n : Symbol]) : Symbol
  (match n
    ['assertEqual '|self.assertEquals|]
    ['assertTrue '|self.assertTrue|]
    ['assertFalse '|self.assertFalse|]
    ['assertRaises '|self.assertRaises|]
    [other other]))

(require typed/rackunit)
(check-equal? (py-flatten '(b c d)) "b(c, d)")

;; given a list of lists of lines,
;; return a list of lines, with blank lines between
;; each block
(define (flattenblocks [blocks : (Listof Block)]) : Block
  (apply append (add-between blocks '(""))))


;; given a classname and a list of fields, return
;; a block representing a python class definition
(define (classdef [classname : Symbol]
                  [fields : (Listof Symbol)]) : Block
  (block-indent
   (cons
    (~a "class "classname"():")
    (flattenblocks
     (list
      (init fields)
      (reprdef classname fields)
      (eqdef classname fields)
      (neqdef))))))

;; given a list of fields, return a block representing
;; a standard __init__ function
(define (init [fields : (Listof Symbol)]) : Block
  (py-def `(__init__ self ,@fields)
          (cons 'block
                (for/list : Block ([a (in-list fields)])
                  (~a "self." a " = "a)))))

;; given a list of fields, return a block representing
;; a standard __repr__ function
(define (reprdef [classname : Symbol]
                 [fields : (Listof Symbol)]) : Block
  (define thisfields : (Listof String)
    (for/list : (Listof String) ([f (in-list fields)])
      (~a "self."f)))
  (define formatstrs : (Listof Sexp)
    (for/list ([i (in-range (length fields))]) '(noquote "%r")))
  (py-def '(__repr__ self)
          (list
           'block
           (~a "return \""(py-flatten (cons classname formatstrs))
               "\" % "(arglist thisfields)))))

;; given the classname and a list of fields, return 
(define (eqdef [classname : Symbol]
               [fields : (Listof Symbol)])
  : Block
  (py-def
   '(__eq__ self other)
   (append
    (list 'block
          (~a "return (" (py-flatten
                          `(== (type other) ,classname))))
    (for/list : Block ([a (in-list fields)])
      (~a "and (self."a" == other."a")"))
    (list ")"))))

(define (neqdef) : Block
  (py-def '(__ne__ self other)
          '((return (not (== other self))))))

;; given a name and a list of sexps, produce a "def" line
(define (py-def [args : (Listof Sexp)] [body : (Listof Sexp)]) : Block
  (cons (~a "def " (py-flatten args) ":")
        (map (indentstr 4)
             (apply append (map py-flatten-l body)))))

;; given a path-string and a block, write it to the file
(define (displaytofile [f : Path-String] [block : Block]) : Void
  (call-with-output-file f
    (λ ([port : Output-Port])
      (for-each (λ (l) (displayln l port)) block))
    #:exists 'truncate))

