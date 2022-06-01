#lang pl

#|********************************************************************************************
  ██████╗  ██╗   ██╗ ███████╗ ███████╗ ████████╗ ██╗  ██████╗  ███╗   ██╗     ██████╗
 ██╔═══██╗ ██║   ██║ ██╔════╝ ██╔════╝ ╚══██╔══╝ ██║ ██╔═══██╗ ████╗  ██║     ╚════██╗
 ██║   ██║ ██║   ██║ █████╗   ███████╗    ██║    ██║ ██║   ██║ ██╔██╗ ██║      █████╔╝
 ██║▄▄ ██║ ██║   ██║ ██╔══╝   ╚════██║    ██║    ██║ ██║   ██║ ██║╚██╗██║     ██╔═══╝
 ╚██████╔╝ ╚██████╔╝ ███████╗ ███████║    ██║    ██║ ╚██████╔╝ ██║ ╚████║     ███████╗
  ╚══▀▀═╝   ╚═════╝  ╚══════╝ ╚══════╝    ╚═╝    ╚═╝  ╚═════╝  ╚═╝  ╚═══╝     ╚══════╝
  Eyal Levi & Yoni Escojido

The goal of this question is to build an interpeter for the LE language than in the last
assignmnet we wrote the BNF for.
********************************************************************************************|#

;; LE abstract syntax trees 
(define-type LE = (U LIST ATOM))

;; LIST abstract syntax trees 
(define-type LIST 
  [Append (Listof LIST)]
  [Lst (Listof LE)]
  [Cons LE LIST]
  [NullT])

;; ATOM abstract syntax trees 
(define-type ATOM 
  [NumT Number]
  [Sym Symbol])

;; converts a list of s-expressions into a list of LEs 
(: parse-sexpr->LEs : (Listof Sexpr) -> (Listof LE)) 
(define (parse-sexpr->LEs sexprs) 
   (map parse-sexprLE sexprs))

;; converts a list of s-expressions into a list of LISTs
(: parse-sexpr->LISTs : (Listof Sexpr) -> (Listof LIST)) 
(define (parse-sexpr->LISTs sexprs) 
   (map parse-sexpr->LIST sexprs))

;; converts an s-expression into a LIST
(: parse-sexpr->LIST : Sexpr -> LIST) 
(define (parse-sexpr->LIST sexpr) 
   (let ([ast (parse-sexprLE sexpr)]) 
   (if (LIST? ast) ast 
     (error 'parsesexprLE "expected LIST; got ~s" ast))))

;; converts an s-expression into an LE
(: parse-sexprLE : Sexpr -> LE)  
(define (parse-sexprLE sexpr) 
  (match sexpr 
    [(number: n)         (NumT n)] 
    ['null               (NullT)] 
    [(symbol: s)         (Sym s)] 
    [(cons 'list rest)   (Lst (parse-sexpr->LEs rest))] 
    [(cons 'append rest) (Append (parse-sexpr->LISTs rest))]
    [(list 'cons l r)    (Cons (parse-sexprLE l) (parse-sexpr->LIST r))]
    [else                (error 'parsesexprLE "bad syntax in ~s" sexpr)])) 

;; parses a string containing an LE expression to an LE AST 
(: parseLE : String -> LE)
(define (parseLE str) 
  (parse-sexprLE (string->sexpr str)))

#|----------------------------------------------------------------------
Formal specs for `eval':
eval(N) = N                   ;; for numbers 
eval(Sym) = 'Sym              ;; for symbols 
eval({list E ...}) = (list eval(E) ...)

eval({cons E1 E2}) = if eval(E2) = (list E)
                         then (cons eval(E1) eval(E2)) 
                     else error

eval({append E ...}) = if eval(E) = (list E') for all expressions E
                         then (append eval(E) ...)
----------------------------------------------------------------------|#    
;; evaluates LE expressions by reducing them to numbers 
(: evalLE : LE -> Any) 
(define (evalLE expr) 
  (if (LIST? expr) 
      (cases expr
        [(Lst lst) (map evalLE lst)] 
        [(Cons l r)
         (let ([e2 (evalLE r)])
         (if (list? e2)
             (cons (evalLE l) e2) 
             (error 'evalLE "append argument: expected List got ~s" e2)))]
        [(Append lst) (apply append (eval-append-args lst))]
        [(NullT) null])
      (cases expr 
        [(NumT n) n]
        [(Sym s) s])))

;; evaluates LE expressions by reducing them to lists (see example in the first test)
(: eval-append-args : (Listof LE) -> (Listof (Listof Any)))  
(define (eval-append-args exprs) 
  (if (null? exprs) 
      null 
      (let ([fst-val (evalLE (first exprs))]) 
        (if (list? fst-val) 
            (cons fst-val (eval-append-args (rest exprs))) 
            (error 'evalLE "append argument: expected List got ~s" fst-val)))))

;; evaluate an LE program contained in a string 
(: runLE : String -> Any) 
(define (runLE str) 
  (evalLE (parseLE str)))
 

;;----------------------------------- TESTS -------------------------------------

;; Testing the eval-append-args function:
(test (eval-append-args (list (Lst (list (NumT 1) (NumT 2) (NumT 3))) (Lst (list (NumT 4) (NumT 5))))) =>
      (list (list 1 2 3) (list 4 5)))

;; Testing the parseLE function:
(test (parseLE "null") => (NullT)) 
(test (parseLE "12") => (NumT 12))
(test (parseLE "boo") => (Sym 'boo))
(test (parseLE "{cons 1 {cons two null}}") => (Cons (NumT 1) (Cons (Sym 'two) (NullT)))) 
(test (parseLE "{list 1 2 3}") => (Lst (list (NumT 1) (NumT 2) (NumT 3))))
(test (parseLE "{append {list 1 2 3} {list 4 5}}") =>
      (Append (list (Lst (list (NumT 1) (NumT 2) (NumT 3))) (Lst (list (NumT 4) (NumT 5))))))
(test (parseLE "{list {cons}}") =error> "parsesexprLE: bad syntax in (cons)") 
(test (parseLE "{list {cons 2 1}}") =error> "parsesexprLE: expected LIST; got") 

;; Testing the runLE function:
(test (runLE "null") => null) 
(test (runLE "12") => 12)
(test (runLE "boo") => 'boo)
(test (runLE "{cons 1 {cons two null}}") => '(1 two)) 
(test (runLE "{list 1 2 3}") => '(1 2 3)) 
(test (runLE "{list {cons}}") =error> "parsesexprLE: bad syntax in (cons)") 
(test (runLE "{list {cons 2 1}}") =error> "parsesexprLE: expected LIST; got")
