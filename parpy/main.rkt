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

;; CLEANUP NEEDED IN T, T2, T1

;; given a name and a set of test sexps, generate
;; python tests named test<name>0, test<name>1, etc.
(: t (Symbol Sexp * -> Block))
(define (t name . sexps) : Block
  (t2 name sexps))

(: t2 (Symbol (Listof Sexp) -> Block))
(define (t2 name sexps) : Block
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

;; given a number and a string, indent the string by the number of
;; chars
(define ((indentstr [n : Natural]) [str : String]) : String
  (string-append (apply string (for/list : (Listof Char) ([i n])
                                 #\space))
                 str))

;; given a top-level expression, flatten it into a block.
(define (py-flatten-toplevel [exp : Sexp]) : Block
  (define (err)
    (raise-argument-error 'py-flatten-toplevel
                          "legal top-level stmt" 0 exp))
  (match exp
    [(list '%testclass (? symbol? classname) (list (? symbol?
                                                      testnames)
                                                   lolotest ...)
           ...)
     (testclass classname (map t2
                               (cast testnames (Listof Symbol))
                               (cast lolotest
                                     (Listof (Listof Sexp)))))]
    [(cons '%testclass _) (err)]
    [(list '%% (? string? comment) body)
     (append (comment->block comment)
           (py-flatten-toplevel body))]
    [(list '%% (? string? comment))
     (comment->block comment)]
    [(cons '%% _) (err)]
    ;; flatten a sequence of top-level-stmts
    [(list '%begin stmts ...)
     (apply append (map py-flatten-toplevel stmts))]
    [(cons '%begin _) (err)]
    [(list 'import (? symbol? module))
     (list (string-append "import " (symbol->string module)))]
    [(cons 'import _) (err)]
    [(list 'class (? symbol? classname) (list (? symbol? fields) ...))
     (classdef classname (cast fields (Listof Symbol)))]
    [(cons 'class _) (err)]
    [_ (py-flatten-stmt exp)]))

;; flatten a list of stmt
(define (py-flatten-stmts [stmts : (Listof Sexp)]) : Block
  (apply append (map py-flatten-stmt stmts)))

;; flatten an s-exp representing a stmt, put it in a list
(define (py-flatten-stmt [stmt : Sexp]) : Block
  (define (err)
    (raise-argument-error 'py-flatten-stmt
                          "legal stmt" 0 stmt))
  (match stmt
    ;; allows escaping back into list of strings:
    [(cons '%block (? block? b)) b]
    [(cons '%block _) (err)]
    ;; flatten a sequence of stmts:
    [(list '%begin stmts ...) (py-flatten-stmts stmts)]
    [(cons '%begin _) (err)]
    [(list 'for (? symbol? loopvar) range bodys ...)
     (block-indent
      (cons (~a "for "loopvar" in "(py-flatten range)":")
            (py-flatten-stmts bodys)))]
    [(cons 'for _) (err)]
    [(list 'while test bodys ...)
     (block-indent
      (cons (~a "while "(py-flatten test)":")
            (py-flatten-stmts bodys)))]
    [(cons 'while _) (err)]
    [(list 'if test (list thens ...))
     (block-indent (cons (~a "if "(py-flatten test)":")
                         (py-flatten-stmts thens)))]
    [(list 'if test (list thens ...) (list elses ...))
     (append
      (block-indent (cons (~a "if "(py-flatten test)":")
                          (py-flatten-stmts thens)))
      (block-indent (cons "else:"
                          (py-flatten-stmts elses))))]
    [(cons 'if _) (err)]
    ;; only lvalues allowable on the left-hand-side of =:
    [(list '= (? lvalue? a) b)
     (list (~a (py-flatten a) " = " (py-flatten b)))]
    [(cons '= _) (err)]
    [(list '%aset! array-exp idx-exp new-val)
     (list (~a (py-flatten array-exp)
               (bracket-wrap (py-flatten idx-exp))
               " = " (py-flatten new-val)))]
    [(cons '%aset! _) (err)]
    [(list '%define (list (? symbol? name) (? symbol? args) ...)
           (? string? docstr)
           bodies ...)
     (py-def (cons name (cast args (Listof Symbol)))
             #:docstring docstr
             bodies)]
    [(list '%define (list (? symbol? name) (? symbol? args) ...)
           bodies ...)
     (py-def (cons name (cast args (Listof Symbol))) bodies)]
    [(cons '%define _) (err)]
    [(list '%% (? string? comment) body)
     (append (comment->block comment)
             (py-flatten-stmt body))]
    [(list '%% (? string? comment))
     (comment->block comment)]
    [(cons '%% _) (err)]
    [(list 'return a)
     (list (space-append "return" (py-flatten a)))]
    [(cons 'return _) (err)]
    [(list 'raise expr)
     (list (space-append "raise" (py-flatten expr)))]
    [(cons 'raise _) (err)]
    ;; testing forms:
    [(list '%check-mut (? symbol? name) init
           call result newval)
     (py-flatten-stmt
      (assert-mut name init call result newval))]
    [(cons '%check-mut _) (err)]
    [(list '%check-selfmut (? symbol? name)
           (list (? symbol? funname) init otherargs ...)
           result)
     (py-flatten-stmt
      (assert-mut name init
                  (cons funname (cons name otherargs))
                  'None
                  result))]
    [(cons '%check-selfmut _) (err)]
    [(list '%check-eq? a b)
     (py-flatten-stmt (list '|self.assertEqual| a b))]
    [(cons '%check-eq? _) (err)]
    [(list '%check-raises? exn fun args ...)
     (py-flatten-stmt
      (append (list '|self.assertRaises| exn fun) args))]
    [(list '%cond clauses ...)
     (py-flatten-stmt
      (unfold-cond clauses))]
    ;; must be an expression used as a stmt:
    [other (list (py-flatten other))]))

;; is this an lvalue?
(define (lvalue? [s : Sexp]) : Boolean
  (match s
    [(? symbol? sym) (legal-id? sym)]
    [(list '%tup (? lvalue? subvals) ...) #t]
    [(list '%o _ (? symbol? sym)) (legal-id? sym)]
    [(list '%sub _ _) #t]
    [else #f]))

;; given a string, return a block representing a comment
(define (comment->block [str : String]) : Block
  (define strs : Block (regexp-split #px"\n" str))
  (map (λ ([l : String]) (string-append "# " l)) strs))


;; given a list of cond clauses, unfold them into a python stmt
(define (unfold-cond [clauses : (Listof Sexp)]) : Sexp
  (match clauses
    ['() (raise-argument-error 'unfold-cond
                               "nonempty list of clauses"
                               0 clauses)]
    [(list _)
     (raise-argument-error 'unfold-cond
                           "cond with more than one clause"
                           0 clauses)]
    ;; irritatingly, must special-case deeper to ensure that result is not
    ;; a list of stmts, but a single stmt; need begin?
    [(list (cons test results) (cons 'else final-results))
     (list 'if test results final-results)]
    [(list (cons test results) _)
     (raise-argument-error 'unfold-cond
                           "cond where final clause is an else"
                           0 clauses)] 
    [(list (cons test results) clauses ...)
     (list 'if test results (list (unfold-cond clauses)))]))

(define-type Infix-Operator
  (U '== '< '<= '% '+ '- '* '/ 'and 'or))
(define-predicate infix-operator? Infix-Operator)

;; convert an s-expression to a python string,
;; e.g. '(a b c) into "a(b, c)"
(define (py-flatten [a : Sexp]) : String
  (define (err)
    (raise-argument-error 'py-flatten
                          "legal expression" 0 a))
  (match a
    [(list (? infix-operator? op) args ...)
     (infixop (symbol->string op) args)]
    [(list 'not a)
     (paren-wrap (space-append "not" (py-flatten a)))]
    [(list '%sub arr idx)
     (string-append
      (py-flatten arr)
      (bracket-wrap (py-flatten idx)))]
    [(cons '%sub _) (err)]
    [(list '%o obj other)
     (string-append (py-flatten obj) "." (py-flatten other))]
    [(cons '%o _) (err)]
    [(list '%noquote (? string? s)) s]
    [(cons '%arr (? list? args))
     (pylist (map py-flatten args))]
    [(cons '%tup (? list? args))
     (arglist (map py-flatten args))]
    [(list 'λ (list (? symbol? args) ...) body)
     (paren-wrap
      (string-append "lambda " (commasep
                                (map symbol->string
                                     (cast args (Listof Symbol))))
                     ": " (py-flatten body)))]
    [(cons 'λ _) (err)]
    [(list (? symbol? fn) args ...)
     (define funname (pyfunname fn))
     (id-check funname)
     (~a (dots-convert (symbol->string funname))
         (arglist (py-flatten-args args)))]
    [(? list? l) (err)]
    [(? string? s) (string-encode s)]
    [(? symbol? s)
     (id-check s)
     (dots-convert (symbol->string s))]
    [(? boolean? b)
     (cond [b "True"]
           [else "False"])]
    [other (~a a)]))

;; given a string representing an identifier, convert >'s to .'s
(define (dots-convert [s : String]) : String
  (regexp-replace* #px">" s "."))

;; given a string, return the string that encodes it in Python
(define (string-encode [s : String]) : String
  (define body-strs : (Listof String)
    (map char-encode (string->list s)))
  (string-append "\"" (apply string-append body-strs) "\""))

;; given a string, return a docstring that encodes it in Python
;; note that not all of the special chars need to be encoded,
;; but I believe that it doesn't hurt.
(define (docstring-encode [s : String]) : String
  (define body-strs : (Listof String)
    (map char-encode (string->list s)))
  (string-append "\"\"\"" (apply string-append body-strs) "\"\"\""))

;; given a character, produce the string that encodes it
;; (possibly incomplete)
(define (char-encode [ch : Char]) : String
  (match ch
    [#\newline "\\n"]
    [#\\ "\\"]
    [#\" "\\\""]
    [otherchar (string otherchar)]))

;; flatten args; if we see a keyword, turn it into a
;; foo=bar pair. So, e.g., '(3 4 #:abc 5 6) => '("3" "4" "abc=5" "6")
(define (py-flatten-args [args : (Listof Sexp)]) : (Listof String)
  (match args
    ['() '()]
    [(list (? keyword? k) val rests ...)
     (cons (string-append (keyword->string k) "=" (py-flatten val))
           (py-flatten-args rests))]
    [(cons f r) (cons (py-flatten f) (py-flatten-args r))]))

(check-equal? (py-flatten-args '(3 4 #:abc 5 6))
              '("3" "4" "abc=5" "6"))
(check-equal? (py-flatten-args '(3 4 #:abc 5 #:zz 6))
              '("3" "4" "abc=5" "zz=6"))

;; signal an error if the given identifier is not a legal python identifier
;; note: this is intentionally very restrictive, to simplify reasoning
;; about what can and can't be an identifier.
;; NOTE : this one *DOES* allow >'s, which will later be converted to dots
(define (id-check [s : Symbol]) : Void
  (unless (legal-id? s)
    (raise-argument-error 'id-check
                          "symbol legal as python identifier"
                          0 s)))

;; is this a legal id?
(define (legal-id? [s : Symbol]) : Boolean
  (regexp-match? #px"^[.a-zA-Z0-9_]([.>a-zA-Z0-9_]*[.a-zA-Z0-9_])?$"
                 (symbol->string s)))

;; no >'s please
(define (base-id? [s : Symbol]) : Boolean
  (regexp-match? #px"^[.a-zA-Z0-9_]+$"
                 (symbol->string s)))

(check-equal? (lvalue? 'abc) #t)
(check-equal? (lvalue? 'abc>def) #t)
(check-equal? (lvalue? '(%tup (%sub (f x) g) (%o 3 zz))) #t)
(check-equal? (lvalue? '(%tup (%sub (f x) g) (f x))) #f)

(check-equal? (dots-convert "bb>ac>d")
              "bb.ac.d")
(check-exn #px"legal as python identifier"
           (λ () (id-check 'abcd>)))
(check-equal? (id-check 'b) (void))

;; a mutation check. Ensure that after evaluating the expression,
;; the original value has been mutated to the later one.
(define (assert-mut [name : Symbol] [initval : Sexp]
                    [call : Sexp] [callresult : Sexp]
                    [expected : Sexp]) : Sexp
  `(%begin
    (= ,name ,initval)
    (%check-eq? ,call ,callresult)
    (%check-eq? ,name ,expected)))

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
(: infixop (String (Listof Sexp) -> String))
(define (infixop op args)
  (paren-wrap
   (apply space-append (add-between (map py-flatten args) op))))

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

;; getting rid of this...
;; convert various symbols to other strings
(define (pyfunname [n : Symbol]) : Symbol
  (match n
    ['assertTrue '|self.assertTrue|]
    ['assertFalse '|self.assertFalse|]
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
          (for/list ([a (in-list fields)])
            `(= (%o self ,a) ,a))))

;; given a list of fields, return a block representing
;; a standard __repr__ function
(define (reprdef [classname : Symbol]
                 [fields : (Listof Symbol)]) : Block
  (define thisfields
    (for/list : (Listof Symbol) ([f (in-list fields)])
      (string->symbol (~a "self."f))))
  (define formatstrs : (Listof Sexp)
    (for/list ([i (in-range (length fields))]) '(%noquote "%r")))
  (py-def '(__repr__ self)
          (list
           ;; note freaky use of py-flatten result as string!
           `(% ,(py-flatten (cons classname formatstrs))
               (%tup ,@thisfields)))))



;; given the classname and a list of fields, return 
(define (eqdef [classname : Symbol]
               [fields : (Listof Symbol)])
  : Block
  (py-def '(__eq__ self other)
          `((and
             (== (type other) ,classname)
             ,@(for/list : (Listof Sexp) ([a (in-list fields)])
                 (ann `(== (%o self ,a) (%o other ,a))
                      Sexp))))
          #;(list
           (cons '%block
                 (append
                  (list
                   (~a "return (" (py-flatten
                                   `(== (type other) ,classname))))
                  (for/list : Block ([a (in-list fields)])
                    (~a "  and (self."a" == other."a")"))
                  (list ")"))))))

(define (neqdef) : Block
  (py-def '(__ne__ self other)
          '((not (== other self)))))

;; given a name and a list of sexps, produce a "def" line
(define (py-def [args : (Listof Sexp)]
                #:docstring [docstr : (U String #f) #f]
                [body : (Listof Sexp)]) : Block
  (define withret (add-return body))
  (define stmt-strs (py-flatten-stmts withret))
  (define body-block (cond [docstr (cons (docstring-encode docstr)
                                         stmt-strs)]
                           [else stmt-strs]))
  (cons (~a "def " (py-flatten args) ":")
        (map (indentstr 4) body-block)))

;; given a list of statements, add 'return' to the expressions
;; in tail position
(define (add-return [stmts : (Listof Sexp)]) : (Listof Sexp)
  (match stmts
    [(cons l '()) (list (add-return-stmt l))]
    [(cons f r) (cons f (add-return r))]))

;; given a statement, add 'return' to the expressions in tail
;; position
(define (add-return-stmt [stmt : Sexp]) : Sexp
  (define (give-up)
    (fprintf (current-error-port)
             "giving up on adding return to stmt: ~e\n"
             stmt)
    stmt)
  (define (err)
    (raise-argument-error 'add-return-stmt
                          "legal stmt" 0 stmt))
  (match stmt
    [(cons '%block _) (give-up)]
    [(list '%begin stmts ...)
     (cons '%begin (add-return stmts))]
    [(cons '%begin _) (err)]
    [(cons 'for _) stmt]
    [(cons 'while _) stmt]
    [(list 'if test (list thens ...))
     (add-return-stmt `(if ,test ,thens (None)))]
    [(list 'if test (list thens ...) (list elses ...))
     (list 'if test (add-return thens) (add-return elses))]
    [(cons 'if _) (err)]
    [(cons '= _) stmt]
    [(list '%aset! array-exp idx-exp new-val) stmt]
    [(list '%define (list (? symbol? name) (? symbol? args) ...)
           (? string? docstr)
           bodies ...)
     stmt]
    [(list '%define (list (? symbol? name) (? symbol? args) ...)
           bodies ...)
     stmt]
    [(cons '%% _) stmt]
    [(cons 'return _) stmt]
    [(cons 'raise _) stmt]
    [(cons '%check-mut _) stmt]
    [(cons '%check-selfmut _) stmt]
    [(cons '%check-eq? _) stmt]
    [(cons '%check-raises? _) stmt]
    [(list '%cond (list clause-elts ...) ...)
     ;; NB treating clauses as lists of statements okay only because
     ;; add-return ignores all but last element of list:
     (cons '%cond (map add-return (cast clause-elts
                                        (Listof (Listof Sexp)))))]
    [(cons '%cond _) (err)]
    ;; must be an expression used as a stmt:
    [other (list 'return stmt)]))

;; given a path-string and a block, write it to the file
(define (displaytofile [f : Path-String] [block : Block]) : Void
  (call-with-output-file f
    (λ ([port : Output-Port])
      (for-each (λ (l) (displayln l port)) block))
    #:exists 'truncate))


(check-not-exn (λ () (assert-mut 'zz 342 '(f zz) "boo" 343)))

(check-equal? (py-flatten '(%tup 3 "bc")) "(3, \"bc\")")

(check-equal?
 (reprdef 'zig '(zag zazz))
 '("def __repr__(self):"
   "    return (\"zig(%r, %r)\" % (self.zag, self.zazz))"))

(check-equal?
 (py-flatten-stmt
  '(%% "swap two elements in an array"
       (%define (swap_elts obj idx1 idx2)
         (= temp (%sub obj idx1))
         (%aset! obj idx1 (%sub obj idx2))
         (%aset! obj idx2 temp))))
 (cons "# swap two elements in an array"
       (py-def '(swap_elts obj idx1 idx2)
               '((= temp (%sub obj idx1))
                 (%aset! obj idx1 (%sub obj idx2))
                 (%aset! obj idx2 temp)))))

(check-equal?
 (py-flatten-stmt
  '(%check-selfmut o (insert_h (%arr 3 9 12 11 4 9) 3 11)
                   (%arr 3 9 11 12 4 9)))
 (py-flatten-stmt
  (assert-mut 'o '(%arr 3 9 12 11 4 9)
              '(insert_h o 3 11)  'None
              '(%arr 3 9 11 12 4 9))))

;; regression test:
(check-equal?
 (py-flatten-stmt
  (assert-mut 'o '(%arr 3 9 12 11 4 9)
              '(insert_h o 3 11)  'None
              '(%arr 3 9 11 12 4 9)))
 (py-flatten-stmt
  '(%block
    "o = [3, 9, 12, 11, 4, 9]"
    "self.assertEqual(insert_h(o, 3, 11), None)"
    "self.assertEqual(o, [3, 9, 11, 12, 4, 9])")))



(check-equal?
 (unfold-cond
  '([(< zig zay) 123]
    [tuvalu "bongo boy"]
    [else (+ 3 4)]))
 '(if (< zig zay)
      (123)
      ((if tuvalu
          ("bongo boy")
          ((+ 3 4))))))

(check-equal?
 (py-flatten-stmt
  '(%cond [(< zig zay) 123]
          [tuvalu "bongo boy"]
          [else (+ 3 4)]))
 (py-flatten-stmt
  '(if (< zig zay)
       (123)
       ((if tuvalu
            ("bongo boy")
            ((+ 3 4)))))))

(check-equal?
 (py-flatten-stmt '(%aset! (f x) (g x) (+ 3 4)))
 '("f(x)[g(x)] = (3 + 4)"))


(check-equal?
 (py-flatten-stmt
  '(%check-raises? IndexError f 34 78 1))
 '("self.assertRaises(IndexError, f, 34, 78, 1)"))

;; essentially a regression test:
(check-equal?
 (py-flatten-toplevel
  '(%testclass
    Lab2Tests
    [SelectionSort
     (%check-eq? (selection_sort (%arr 9 8 18 7 8))
               (%arr 7 8 8 9 18))
     (%check-eq? (move_smallest_to_posn (%arr 9 8 18 7 8) 2)
               (%arr 9 8 7 18 8))]
    [InsertionSort
     (%check-selfmut o (insert (%arr 3 9 12 11 4 9) 3 11)
                     (%arr 3 9 11 12 4 9))
     (%check-selfmut o (insert (%arr 3 9 12 12 4 9) 2 11)
                     (%arr 3 9 11 12 4 9))]))
 (testclass
  'Lab2Tests
  (list
   (t 'SelectionSort
      '(%check-eq? (selection_sort (%arr 9 8 18 7 8))
                (%arr 7 8 8 9 18))
      '(%check-eq? (move_smallest_to_posn (%arr 9 8 18 7 8) 2)
                (%arr 9 8 7 18 8)))
   (t 'InsertionSort
      '(%check-selfmut o (insert (%arr 3 9 12 11 4 9) 3 11)
                       (%arr 3 9 11 12 4 9))
      '(%check-selfmut o (insert (%arr 3 9 12 12 4 9) 2 11)
                       (%arr 3 9 11 12 4 9))))))

(check-equal? (py-flatten '(or 3 4)) "(3 or 4)")

(check-equal? (py-flatten '(%o def ghi)) "def.ghi")
(check-equal? (py-flatten '(%o def (ghi jkl))) "def.ghi(jkl)")
(check-equal? (py-flatten "abc\r\n\t\\\"def")
              "\"abc\r\\n\t\\\\\"def\"")

(check-equal? (py-flatten-stmt '(while (< x 3)
                                  (print x)))
              (list "while (x < 3):" "    print(x)"))

;; regression:
(check-equal?
 (py-flatten-toplevel '(class Cons (first rest)))
 '("class Cons():"
   "    def __init__(self, first, rest):"
   "        self.first = first"
   "        self.rest = rest"
   "    "
   "    def __repr__(self):"
   "        return (\"Cons(%r, %r)\" % (self.first, self.rest))"
   "    "
   "    def __eq__(self, other):"
   "        return ((type(other) == Cons) and (self.first == other.first) and (self.rest == other.rest))"
   "    "
   "    def __ne__(self, other):"
   "        return (not (other == self))"))


(check-equal? (comment->block "abcd") '("# abcd"))
(check-equal? (comment->block "abcd\nefg")
              '("# abcd" "# efg"))

(check-equal? (py-flatten '(or #f #t)) "(False or True)")

(check-equal? (py-flatten '(λ (a b) (< a b)))
              "(lambda a, b: (a < b))")

(check-equal? (docstring-encode "abc\"\"\"def")
              "\"\"\"abc\\\"\\\"\\\"def\"\"\"")

(check-equal? (py-def '(zig zag) #:docstring "do the thing" '(17))
              '("def zig(zag):"
                "    \"\"\"do the thing\"\"\""
                "    return 17"))

(check-equal? (add-return-stmt '(f 3 3))
              '(return (f 3 3)))
(check-equal? (add-return-stmt '(if (< x 3) ((+ 4 5))))
              '(if (< x 3) ((return (+ 4 5))) ((return None))))
(check-equal? (add-return-stmt '(%begin 3 4 5))
              '(%begin 3 4 (return 5)))
(check-equal? (add-return-stmt '(%cond [(< pre_elt elt)
                                        (%aset! arr idx elt)
                                        None]
                                       [else
                                        (%aset! arr idx pre_elt)]))
              '(%cond [(< pre_elt elt)
                       (%aset! arr idx elt)
                       (return None)]
                      [else
                       (%aset! arr idx pre_elt)]))

(check-equal? (py-flatten '(and 3 4 5)) "(3 and 4 and 5)")
(check-equal? (infixop "==" (list 'abc "def")) "(abc == \"def\")")

(check-equal? (py-flatten-stmt '(raise IndexError))
              '("raise IndexError"))

(check-equal? (add-return-stmt '(return 19)) '(return 19))
(check-equal? (add-return-stmt '(raise 23847)) '(raise 23847))