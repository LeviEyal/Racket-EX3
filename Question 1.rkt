#lang pl
#|********************************************************************************************
  ██████╗  ██╗   ██╗ ███████╗ ███████╗ ████████╗ ██╗  ██████╗  ███╗   ██╗      ██╗
 ██╔═══██╗ ██║   ██║ ██╔════╝ ██╔════╝ ╚══██╔══╝ ██║ ██╔═══██╗ ████╗  ██║     ███║
 ██║   ██║ ██║   ██║ █████╗   ███████╗    ██║    ██║ ██║   ██║ ██╔██╗ ██║     ╚██║
 ██║▄▄ ██║ ██║   ██║ ██╔══╝   ╚════██║    ██║    ██║ ██║   ██║ ██║╚██╗██║      ██║
 ╚██████╔╝ ╚██████╔╝ ███████╗ ███████║    ██║    ██║ ╚██████╔╝ ██║ ╚████║      ██║
  ╚══▀▀═╝   ╚═════╝  ╚══════╝ ╚══════╝    ╚═╝    ╚═╝  ╚═════╝  ╚═╝  ╚═══╝      ╚═╝
  Eyal Levi & Yoni Escojido
********************************************************************************************

The extended AE language:

Two diffrences from the original AE language:
        1) It is in potfix form rather than prefix form,
           meaning that any expression is in the form: {<operand> <operand> <operator>}
        2) It has two new operators: 'power' and 'sqr'.

BNF for the extended AE language:
   <AE> ::= <num>
          | { <AE> <AE> + }
          | { <AE> <AE> - }
          | { <AE> <AE> * }
          | { <AE> <AE> / }
          | { <AE> <AE> power }
          | { <AE> <AE> sqr }

This quistion was very easy and it took us about half an hour more or less.
|#

;; AE abstract syntax trees
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE]
  [Pow AE AE]
  [Sqr AE])

;; parses s-expressions into AEs
(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list l r '+)     (Add (parse-sexpr l)(parse-sexpr r))]
    [(list l r '-)     (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list l r '*)     (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list l r '/)     (Div (parse-sexpr l)(parse-sexpr r))]
    [(list l r 'power) (Pow (parse-sexpr l)(parse-sexpr r))]
    [(list n 'sqr)     (Sqr (parse-sexpr n))]
    [else              (error 'parse-sexpr "bad syntax in ~s" sxp)]))


;; parses a string containing an AE expression to an AE AST
(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))

;; consumes an AE and computes the corresponding number
(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(Pow l r) (pow (eval l) (eval r))]
    [(Sqr n) (* (eval n) (eval n))]))

;; Recursive function that computes the result of a^b for some number a and an integer b.
(: pow : Number Number -> Number)
(define (pow a b)
  (cond
    [(not (integer? b)) (error 'eval "power expects an integer power, got ~s" b)]
    [(> b 0) (* a (pow a (- b 1)))]
    [else 1]))

;; evaluate an AE program contained in a string
(: run : String -> Number)
(define (run code)
  (eval (parse code)))

;;----------------------------------- TESTS -------------------------------------
(test (run "3") => 3) 
(test (run "{3 4 +}") => 7) 
(test (run "{{3 4 -} 7 +}") => 6) 
(test (run "{{3 4 power} 7 +}") => 88)
(test (run "{{3 4/3 power} 7 +}") =error> "eval: power expects an integer power, got")
(test (run "{{3 0 power} 7 +}") => 8)

(test (run "{{2 4 power} {5 sqr} +}") => 41) 
(test (run "{{2 4/5 power} {5 sqr} +}") =error> "eval: power expects an integer power, got")