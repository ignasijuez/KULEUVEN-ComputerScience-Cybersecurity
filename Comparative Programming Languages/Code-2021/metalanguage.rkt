#lang racket
(require eopl)
(require rackunit)

;; This file introduces the subset of Racket that we will use as
;; metalanguage for implementing programming language interpreters

;; This file summarizes the main aspects of Chapters 1 and 2 that
;; you need to understand for this course

#|

The basic ingredients of the meta-language are:

1. Primitive types:
 - Integers: 1, 2, 3, -1, 23454235234523452352345
 - Booleans: #t, #f
 - Symbols: 'x, 'y, 'identifier, '+

2. Operations: (think of them as predefined functions for now)
 +, -, <=, ...
 if, and, or, ...
 eqv?
 Dynamic type testing: number?, symbol?, boolean?
 Syntax for function application: (f e1 e2 …)


3. Definitions:
 Constants:
  (define id exp)
 Functions (think of these as two alternative syntaxes for now):
  (define (f p1 p2 ...) exp)
  (define f (lambda (p1 p2 ...) exp))

 - Note that f can now be used like predefined functions + en -
 - It is possible to redefine primitive functions (DON’T DO THIS!)
 - functions can be recursive and mutually recursive

|#

(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))

(define (even n)
  (if (= n 0)
      #t
      (odd (- n 1))))

(define (odd n)
  (if (= n 0)
      #f
      (even (- n 1))))

(define (plus2 n) (+ 2 n))
(define pi 3.14)

#|
A more precise definition of the syntax:

expr := atom
     |  (exp1 exp2 exp3 …)
     |  (keyword …)

atom := literal
     | 	identifier

Keywords we will use (for now):
 define
 if, cond, or, and (WHY!)
 (lambda - but wait for a real definition of what it means)

Evaluation rules:
 (exp1 exp2 exp3 ...) -> evaluate all expressions, and call the value of exp1 with as
                         parameters the values of exp2,exp3,...
 (keyword ...) -> depends on the keyword

NOTE: this implies that superfluous parentheses leads to errors!

Finally, to represent composite data, the meta-language includes:

4. Heterogeneous lists:
 The list constructors: list, cons, null
 List operations: null?, car, cdr, list?
|#

;; list-length : List -> Int
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

;; nth-element : List * Int -> SchemeVal
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error "List too short." )))

;; create-range: Int x Int -> Listof(Int)
(define (create-range n m)
  (if (< m n) '()
      (cons n (create-range (+ n 1) m))))

; NOTE: there is a built-in Racket function range that does the same and more

#|

Useful notation for lists: quoting
(quote (1 2 3 4))
(quote ((+ 1 2) 1 2))
(quote (if #t 1 2))
Shorthand: ‘(1 2 3)

|#

(define list1
  (list 1 (+ 1 2) (number? 3)))

(define list2
  (quote (1 (+ 1 2) (number? 3))))


;; NOTE: this is a good point to break, and to do a bit of practice programming in the
;; meta-language. E.g. define multiplication in terms of addition, remove all symbols
;; from a list, ...
;;
;; ----------------------------EXERCISES--------------------------------------------------------

;;standard multiplication
(define (multiplication1 n m) (* n m))

;;multiplication in terms of addition //HERE in multiplication2 invocation we cant use () for the parameters! (- n 1) NOT ((- n 1) m) LIKE (cons n (create-range (+ n 1) m))
(define (multiplication2 n m) ;; 3x2
  (if (eqv? 0 n) ;;if n== 0 > 3
      0 ;;if
      (+ m (multiplication2 ((- n 1) m))))) ;;else 2+ multiplication2((3-1)*2) 

(define multiplication3
  (lambda (n m)
  (if (eqv? 0 n) ;;if n== 0 > 3
      0 ;;if
      (+ m (multiplication3 (- n 1) m))))) ;;else 2+ multiplication2((3-1)*2)

;;remove all symbols from a list
(define (rm_sym2 lst)
    (if (null? lst)
        '()
        (if (symbol? (car lst))
            (rm_sym2 (cdr lst)) ;;if true -> don't add symbol to answer
            (cons (car lst) (rm_sym2 (cdr lst)))))) ;; else add type to answer

;;ALT:
(define rm_sym
  (lambda (lst)
    (if (null? lst)
        '()
        (if (symbol? (car lst))
            (rm_sym (cdr lst)) ;;if true -> don't add symbol to answer
            (cons (car lst) (rm_sym (cdr lst))))))) ;; else add type to answer

;; --------------------------END-OF-EXERCISES---------------------------------------------------
#|

The meta-language is a sub-language of Racket.
Hence:
 - we can use Racket as an interpreter for the meta-language
   (Beware! This can easily lead to confusion!)
   (Beware! The textbook does not draw a clear boundary!)

 - it is easy to use the full power of Racket to
   interact with meta-language programs and data, for
   instance for unit testing or visualization of metaprograms

 - we can "grow" the meta-language by including more features
   (and we will do this several times!)

What follows are some Racket examples (these are NOT in the meta-language!)

|#

; Unit testing helpers: check-equal?

(check-equal? (fac 1) 1)

; higher-order functions: map, filter, foldl, foldr

(define list3
  (range 1 10))

;(map fac list3)
;(filter odd list3)
;(foldl cons null l3)
;(foldl op a0 '(a1 a2 ... aN))   ; fold left
; == (op aN ... (op a2 (op a1 a0)))
; == aN op ( ... (a2 op (a1 op a0)))
;
; foldr (fold right) does the same but takes elements of the list from the right

; anonymous functions, lambda expressions

;(map (lambda (n) (+ n 1)) list3)
;(filter (lambda (n) (< n 5)) list3)

; programming with pictures
(require pict)
; (circle 30)
; (disk 30)
; (vc-append (circle 30) (disk 30))
; + combine with higher-order programming
;
; This can be very useful for visualizing results of meta-language
; programs


;; Some programming exercises in the meta-language

;; remove-first : Sym * Listof(Sym) -> Listof(Sym)
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))


(check-equal? (remove-first 'a '(a b c)) '(b c))
(check-equal? (remove-first 'b '(e f g)) '(e f g))
(check-equal? (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
(check-equal? (remove-first 'x '()) '())

;; S-list ::= ( S-exp* )
;; S-exp  ::= Symbol | S-list
;; subst : Sym * Sym * S-list -> S-list

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (subst-in-s-exp new old (car slist))
         (subst new old (cdr slist))))))

;; subst-in-s-exp : Sym * Sym * S-exp -> S-exp
;; Page: 21
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))

(check-equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))

;; number-elements-from : Listof(SchemeVal) * Int ->
;;                           Listof(List(Int,SchemeVal))
;; usage: (number-elements-from '(v0 v1 v2  ...) n)
;;         = ((n v0 ) (n+1 v1) (n+2 v2) ...)

(define number-elements-from
  (lambda (lst n)
    (if (null? lst) '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))

;; number-elements : List -> Listof(List(Int,SchemeVal))

(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

(check-equal? (number-elements '(a b c d e)) '((0 a) (1 b) (2 c) (3 d) (4 e)))

;; list-sum : Listof(Int) -> Int
;; Page: 24
(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

(check-equal? (list-sum (list 1 2 3 4 5)) 15)

;; ----------------------------EXERCISES--------------------------------------------------------

;; remove-first2 : Sym * Listof(Sym) -> Listof(Sym)
(define remove-first2
  (lambda (s lst)
    (if (null? lst)
        '()
        (if (eqv? s (car lst))
            (cdr lst)
            (cons (car lst) (remove-first2 s (cdr lst)))))))

(check-equal? (remove-first2 'a '(a b c)) '(b c))
(check-equal? (remove-first2 'b '(e f g)) '(e f g))
(check-equal? (remove-first2 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
(check-equal? (remove-first2 'x '()) '())



;; S-list ::= ( S-exp* )
;; S-exp  ::= Symbol | S-list
;; subst2 : Sym * Sym * S-list -> S-list
(define subst2
  (lambda (new old lst)
    (if (null? lst)
        '()
        (if (eqv? old (car lst))
            (cons new (subst2 new old (cdr lst)))
            (cons (car lst) (subst2 new old (cdr lst)))))))


;; subst-in-s-exp : Sym * Sym * S-exp -> S-exp
;; Page: 21
;;not needed


;; number-elements-from : Listof(SchemeVal) * Int ->
;;                           Listof(List(Int,SchemeVal))
;; usage: (number-elements-from '(v0 v1 v2  ...) n)
;;         = ((n v0 ) (n+1 v1) (n+2 v2) ...)

;; number-elements : List -> Listof(List(Int,SchemeVal))


;; list-sum : Listof(Int) -> Int
;; Page: 24
(define list-sum2
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (list-sum2 (cdr lst))))))


;; --------------------------END-OF-EXERCISES---------------------------------------------------

;; ---------------------------------------------------------------------
;;
;; An implementation of the lambda-calculus expression datatype
;; Lc-exp ::= Symbol | (lambda (Symbol) Lc-exp) | (Lc-exp Lc-exp)
;;
;; Think of this as a first example of an OBJECT LANGUAGE
;; Caveat: it is confusing that both meta-language and object language
;; are Racket subsets.

;; Some example "object programs":

(define id
  (lambda (x) x))

(define twice
  (lambda (f) (lambda (x) (f (f x)))))

(define id_applied_to_id
  ((lambda (x) x) (lambda (x) x)))

; uncommenting the lines below leads to an infinite loop
;
;(define loop
;  ((lambda (x) (x x)) (lambda (x) (x x))))

;; One interesting question about lambda-calculus expressions is what
;; variables occur free in them
;;
;; informally: a variable x occurs free in exp if it occurs in exp, not
;; hidden under a nested lambda (x)

;; occurs-free? : Symbol * Lc-exp -> Bool
;; usage:
;;   returns #t if the symbol var occurs free in exp,
;;   otherwise returns #f.

;; renamed to occurs-free-old? to avoid name clash with later definition

(define occurs-free-old?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))  ;LcExp::= symbol
      ((eqv? (car exp) 'lambda)       ;LxExp::= (lambda (symbol) Lc-Exp)
       (and
        (not (eqv? var (car (cadr exp))))  ;(car (cadr exp)) is the bound var // var cant be (symbol)
        (occurs-free-old? var (caddr exp)))) ; (caddr exp) is the body        // var must not appear in Lc-Exp (3rd item in definition)
      (else
       (or
        (occurs-free-old? var (car exp))
        (occurs-free-old? var (cadr exp)))))))


(check-equal? (occurs-free-old? 'x 'x) #t)
(check-equal? (occurs-free-old? 'x 'y) #f)
(check-equal? (occurs-free-old? 'x '(lambda (x) (x y))) #f)
(check-equal? (occurs-free-old? 'x '(lambda (y) (x y))) #t)
(check-equal? (occurs-free-old? 'x '((lambda (x) x) (x y))) #t)
(check-equal? (occurs-free-old? 'x '(lambda (y) (lambda (z) (x (y z))))) #t)

;============================================================================
; Data abstraction
;
; Look back at the implementation of occurs-free above:
;  - implementation details are exposed
;  - hard to read
;
; Data abstraction enforces a separation between an *implementation* and an *interface*
;  - hides implementation details => easier to evolve the implementation
;  - easier to read: clients only need to understand the interface

; The book uses informal but precise interface specifications
; E.g. specification of the natural numbers, page 32

;; Two different implementations of the natural numbers interface
(define (zero) 0)
(define (is-zero? n) (if (eqv? n 0) #t #f))
(define (successor n) (+ n 1))
(define (predecessor n) (+ n -1))

;(define (zero) '())
;(define (is-zero? n) (null? n))
;(define (successor n) (cons 0 n))
;(define (predecessor n) (cdr n))

(check-equal? (is-zero? (zero)) #t)
(check-equal? (is-zero? (successor (zero))) #f)
(check-equal? (is-zero? (predecessor (successor (zero)))) #t)


; Specification of the environment interface: page 36

;; An implementation of the environment interface
;;
;; Env ::= (empty-env) | (extend-env Symbol Val Env)
(define (empty-env) '(empty-env))
(define (extend-env var val  env) (list 'extend-env var val env))
(define (apply-env env search-var)
  (cond [(eqv? (first env) 'empty-env)
         (eopl:error "Unbound var")]
        [(eqv? search-var (second env))
         (third env)]
        [else
         (apply-env (fourth env) search-var)]))

(define e1
  (extend-env 'x 1
              (extend-env 'y 2
                          (extend-env 'x 3
                                      (empty-env)))))


;; Section 2.4 in the textbook introduces the final extension of the meta-language:
;; convenient syntax for defining recursive data types.

;; The datatype of lambda-expressions

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;; Pattern matching
;; occurs-free? : Symbol * Lc-exp -> Bool
(define occurs-free?
  (lambda (search-var exp) ;if search-var occurs free in exp
    (cases lc-exp exp ;exp is type lc-exp (lambda calculus)
      (var-exp (var) (eqv? var search-var)) ;var-exp (var) -> implies that (var) will be the var in the form var-exp
      (lambda-exp (bound-var body) ;(bound-var body) -> implies that their will be the inside of the lambda-exp
                  (and
                   (not (eqv? search-var bound-var))
                   (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or
                (occurs-free? search-var rator)
                (occurs-free? search-var rand))))))

;; test items
(define t1 (var-exp 'x))
(check-equal? (occurs-free? 'x t1) #t)

(define t2 (var-exp 'y))
(check-equal? (occurs-free? 'x t2) #f)

(define t3
  (lambda-exp 'x
              (app-exp (var-exp 'x) (var-exp 'y))))
(check-equal? (occurs-free? 'x t3) #f)

(define t4
  (lambda-exp 'y
              (app-exp (var-exp 'x) (var-exp 'y))))
(check-equal? (occurs-free? 'x t4) #t)

(define t5 (app-exp
            (lambda-exp 'x (var-exp 'x))
            (app-exp (var-exp 'x) (var-exp 'y))))
(check-equal? (occurs-free? 'x t5) #t)

(define t6
  (lambda-exp 'y
              (lambda-exp 'z
                          (app-exp (var-exp 'x)
                                   (app-exp (var-exp 'y) (var-exp 'z))))))
(check-equal? (occurs-free? 'x t6) #t)

(define t7
  (lambda-exp 'y
              (lambda-exp 'z
                          (app-exp (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'x)))
                                   (app-exp (var-exp 'y) (var-exp 'z))))))
(define t8
  (lambda-exp 'y
              (lambda-exp 'z
                          (app-exp (var-exp 'x)
                                   (app-exp (var-exp 'y) (var-exp 'z))))))
(define t9 (lambda-exp 'z
                       (app-exp (var-exp 'z)
                                (lambda-exp 'y
                                            (lambda-exp 'z
                                                        (app-exp (var-exp 'y)
                                                                 (app-exp (var-exp 'z) (var-exp 'w))))))))

(define terms (list t1 t2 t3 t4 t5 t6 t7 t8 t9))

; convert abstract syntax to strings

(define lc-exp->string
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var)
               (symbol->string var))
      (lambda-exp (bound-var body)
                  (string-append "(λ ("
                                 (symbol->string bound-var)
                                 ") "
                                 (lc-exp->string body)
                                 ")"))
      (app-exp (rator rand)
               (string-append "("
                              (lc-exp->string rator)
                              " "
                              (lc-exp->string rand)
                              ")")))))


















;; ANYTHING BELOW THIS LINE IS OPTIONAL =====================================

;; Some fun programming with recursive data types: drawing AST's

;; tower: pict -> pict -> pict
;; Draws p1
;;       |
;;       p2
;;
;; NO NEED TO UNDERSTAND THE IMPLEMENTATION OF tower!

(define tower
  (lambda (p1 p2)
    (pin-line (vc-append 30 p1 p2) p1 cb-find p2 ct-find)))

;; triangle : pict -> pict -> pict -> pict
;; Draws      top
;;           /  \
;;        left right
;;
;; NO NEED TO UNDERSTAND THE IMPLEMENTATION OF triangle!

(define triangle
  (lambda (top left right)
    (pin-line (pin-line (vc-append 30 top (hc-append 30 left right)) top cb-find left ct-find) top cb-find right ct-find)))

;; draw-ast : Lcexp -> Pict  ;;(map draw-ast terms)
(define draw-ast
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var)
               (text (symbol->string var) '() 30))
      (lambda-exp (bound-var body)
                  (let*
                      [(top (text (string-append "λ"
                                                 (symbol->string bound-var)) '() 30))
                       (bottom (draw-ast body))]
                    (tower top bottom)))
      (app-exp (rator rand)
               (let*
                   [(top (text "app" '() 30))
                    (left (draw-ast rator))
                    (right (draw-ast rand))]
                 (triangle top left right))))))




;; marking unbound variables

(define draw-ast-marked
  (lambda (exp env)
    (cases lc-exp exp
      (var-exp (var)
               (let [(p (text (symbol->string var) '() 30))]
                 (if (member var env) p (frame p #:color "red"))
                 ))
      (lambda-exp (bound-var body)
                  (let*
                      [(top (text (string-append "λ" (symbol->string bound-var)) '() 30))
                       (bottom (draw-ast-marked body (cons bound-var env)))]
                    (tower top bottom)))
      (app-exp (rator rand)
               (let*
                   [(top (text "app" '() 30))
                    (left (draw-ast-marked rator env))
                    (right (draw-ast-marked rand env))]
                 (triangle top left right))))))

(define (draw-marked exp) (draw-ast-marked exp '()))

;; drawing binding arrows
;; NO NEED TO STUDY THIS CODE - just demo-ing some program-manipulating programs

;; A very simple datatype of environments of the form ((key value) ...)
(define (lookup env sym)
  (if (eqv? sym (car (car env)))
      (cadr (car env))
      (lookup (cdr env) sym)))

(define (mem env sym)
  (if (null? env) #f
      (if (eqv? sym (car (car env))) #t
          (mem (cdr env) sym))))
;; We first annotate the AST with the pict's that make up its tree

(define-datatype lc-exp-ext lc-exp-ext?
  (var-exp-ext
   (var symbol?)
   (p-var pict?))
  (lambda-exp-ext
   (bound-var symbol? )
   (p-bound-var pict?)
   (body lc-exp-ext?)
   (p-lambda pict?))
  (app-exp-ext
   (rator lc-exp-ext?)
   (rand lc-exp-ext?)
   (p-app pict?)))

;; project out the pict of the main expression
(define ppict
  (lambda (exp-ext)
    (cases lc-exp-ext exp-ext
      (var-exp-ext (var p-var) p-var)
      (lambda-exp-ext (bv pbv b pl) pl)
      (app-exp-ext (r1 r2 p) p))))

;; lc-exp-to-ext : Lcexp -> Lc-exp-ext (that associates the appropriate pict with each AST node)
(define lc-exp-to-ext
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) (var-exp-ext var (text (symbol->string var) '() 30)) )
      (lambda-exp (bound-var body)
                  (let*
                      [(top (text (string-append "λ" (symbol->string bound-var)) '() 30))
                       (body-ext (lc-exp-to-ext body))
                       (full (tower top (ppict body-ext)))]
                    (lambda-exp-ext bound-var top body-ext full)
                    ))
      (app-exp (rator rand)
               (let*
                   [(top (text "app" '() 30))
                    (rator-ext (lc-exp-to-ext rator))
                    (rand-ext (lc-exp-to-ext rand))
                    (full (triangle top (ppict rator-ext) (ppict rand-ext)))]
                 (app-exp-ext rator-ext rand-ext full))))))

;; now use this to draw more interesting diagrams, e.g. arrow from use of var to binding location

(define draw-binding
  (lambda (exp-ext env p)
    (cases lc-exp-ext exp-ext
      (var-exp-ext (var p-var) (if (mem env var)
                                   ; draw arrow from p-var to (lookup env var)
                                   (pin-arrow-line 5 p p-var ct-find (lookup env var) cb-find #:color "red" #:start-angle 2 #:end-angle 1)
                                   ; draw a red box around p-var
                                   (pin-over p p-var lt-find (frame p-var #:color "red"))
                                   ))
      (lambda-exp-ext (bv pbv b pl) (draw-binding b (cons (list bv pbv) env) p))
      (app-exp-ext (r1 r2 p2) (draw-binding r1 env (draw-binding r2 env p))))))


(define draw
  (lambda (exp)
    (let [(l (lc-exp-to-ext exp))]
      (draw-binding l '() (ppict l)))))

#|
Q&A session:

example:
 exp::= numbers       1,2,3,4
        [num(n)]
    
    ::= (exp + exp)
        [add(exp+exp)]

#object program
(dfine c1 '(1 + 2))
(define c2 '((1 + 2) + (3 + 4)))

#metalanguage
(define (eval c)
  cond)


|#
;concrete syntax
;(define (eval c)
;  (cond [(number? c) c]
;        [(eqv? (second c) '+) (+ (evalc (first c)) (evalc (third c)))]
;        [else (eopl:error "bad syntax")]))

;abstract syntaxt
(define-datatype exp exp? ;name and smth that test if its that type
  (num (n number?))
  (add (e1 exp?) (e2 exp?)))

(define a1 (add (num 1) (num 2)))
(define a2 (add (add (num 1) (num 2)) (add (num 3) (num 4))))


(define (evala e)
  (cases exp e
    (num (n) n)
    (add (e1 e2) (+ (evala e1) (evala e2)))))


;concrete syntax
(define (parse c)
  (cond [(number? c) (num c)]
        [(eqv? (second c) '+) (add (parse (first c)) (parse (third c)))]
        [else (eopl:error "bad syntax")]))








