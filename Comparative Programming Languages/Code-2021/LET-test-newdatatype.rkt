#lang racket
(require eopl)

;; Syntax
;; Datatype of object programs  -----  pag6 slides
(define-datatype program program?
  (a-program (exp1 expression?))) 

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (zero?-exp (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  ;;new
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  ;;new2
  (proc-exp
   (var symbol?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  )

;; Semantics
;; Datatype of expressed values
(define-datatype expval expval?    ;;represente expressed values that can be num-val or bool-val, exp-val objects can be numerical values or boolean values
  (num-val             ;;name num-val 
   (value number?))    ;;structure num-val has 1 field that is value and this should be a number
  (bool-val            ;;name bool-val
   (boolean boolean?)) ;;structure bool-val has 1 field that is boolean and this should be a boolean
  ;new2
  (prov-val
   (proc proc?))
  )

;new2
;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?  ;;proc definition data-type
  (procedure  
   (var symbol?)
   (body expression?)
   (env environment?))
  )

;; expval->num : ExpVal -> Int
(define expval->num    ;;declares function name that takes expval and returns int
  (lambda (v)          ;;anonymous function that takes argument v
    (cases expval v    ;;expval object v cases
      (num-val (num) num) ;;(num) extracts the value inside num-val and puts the result in "num" then return
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v                ;;idem expval->num
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;;new2
;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-exp (p) p)
      (else (expval-entraxtor-error 'proc v)))))

;; extractor-error
(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; Environments  -->> used to store bindings between symbols (variable names) and values (expressed values)
(define-datatype environment environment? ;;definition of environment with 2 data types: empty-env and extend-env
  (empty-env) ;; empty environment with no bindings
  (extend-env ;; extend environment with a binding: 3 components
   (bvar symbol?) ;; symbol representing var name
   (bval expval?) ;; expval representing associated value
   (saved-env environment?) ;; Another environment, which is the environment that was in effect before this binding was added. This supports a kind of "stack" of environments.
   ))

(define apply-env ;; -->> look up for a symbol and return the associated value
  (lambda (env search-sym) ;; 2 arguments env and search-sym
    (cases environment env ;; environment object env cases
      (empty-env () ;; it is empty
                 (eopl:error 'apply-env "No binding for ~s" search-sym)) ;; return error
      (extend-env (bvar bval saved-env) ;; it is not empty and has the structure of an extend-env with (bvar bval saved-env)
                  (if (eqv? search-sym bvar) ;; bvar is a symbol, if bvar == search-sym that we are looking for 
                      bval ;; returns bval
                      (apply-env saved-env search-sym) ;; otherwise bvar =! search-sym we call again in the saved env
                      )))))

;; init-env : () -> Env
(define init-env 
  (lambda () ;; following the definition of an extend-env we have the (bvar bval saved-env) so bvar=i bval=1 and saved-env is a new extend-env where bvar=v bval=5 and on and on
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)  ;; until the last env where the saved-env stops having more extend-env and it is an empty-env
       (empty-env))))))




;; Interpreter for LET

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm ;; program object pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of ;; all types of value of in this cases
  (lambda (exp env)
    (cases expression exp ;; expression object exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      ;;new
      (mult-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (* num1 num2)))))
      (div-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (if (zero? num2)
                        (eopl:error 'value-of "Division by zero")
                        (num-val (/ num1 num2))))))
      ;;new2
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      (call-exp (rator rand)
                (let (proc (expval->proc (value-of rator env)))
                  (arg (value-of rand env)))
                (apply-env proc arg))
      )))
;new2
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body env)
                 (value-of body (extended-env var val saved-env))))))





;; TESTS FOR NEW IMPLEMENTATIONS

(define my-program
  (a-program
   (mult-exp
    (const-exp 3)
    (div-exp
     (const-exp 6)
     (const-exp 2)))))

(value-of-program my-program)




















;;; ======== ANYTHING BELOW THIS LINE IS SUPPORT CODE =========
(require racket/exn)
; for playing with programs in DrRacket
(define (test p)
  (begin
    (display (program->string p))
    (display "evaluates to:\n")
    (display (with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->string (value-of-program p)) ))
    (display "\n----------------\n")))

;; PRETTY PRINTING
(require (rename-in pprint (empty ppempty)))

(define (program->doc pgm)
  (cases program pgm
    (a-program (exp1)
               (h-append (exp->doc exp1 ) line ))))

(define (exp->doc exp)
  (cases expression exp
    (const-exp (num) (text (number->string num)))
    (var-exp (var) (text (symbol->string var)))
    (diff-exp (exp1 exp2)
              (let ((val1 (exp->doc exp1))
                    (val2 (exp->doc exp2)))
                (group (h-append lparen space (align (h-append val1 line (char #\-) space (align (h-append val2 space rparen))))))))
    (zero?-exp (exp1)
               (let ((val1 (exp->doc exp1 )))
                 (hs-append (kw "zero?") lparen (align val1) rparen)))
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (exp->doc exp1 ))
                  (val2 (exp->doc exp2 ))
                  (val3 (exp->doc exp3 )))
              (group (h-append
                      (kw "if")
                      space
                      (align val1)
                      line
                      (kw "then")
                      space
                      (align val2)
                      line
                      (kw "else")
                      space
                      (align val3) )))          )
    (let-exp (var exp1 body)       
             (let ((val1 (exp->doc exp1))
                   (val2 (exp->doc body)))
               (h-append
                (kw "let")
                space
                (text (symbol->string var))
                space
                equals
                space
                (align val1)
                line
                (kw "in")
                space
                (align val2)) ))
    (mult-exp (exp1 exp2)
              (let ((val1 (exp->doc exp1))
                    (val2 (exp->doc exp2)))
                (group (h-append lparen space (align (h-append val1 space (char #\*) space val2)) rparen))))
    
    (div-exp (exp1 exp2)
             (let ((val1 (exp->doc exp1))
                   (val2 (exp->doc exp2)))
               (group (h-append lparen space (align (h-append val1 space (char #\/) space val2)) rparen))))
      
    ))

(define (program->string p)
  (pretty-format (program->doc p)))

(define (expval->string v)
  (cases expval v
    (bool-val (bool) (if bool "#t" "#f"))
    (num-val (n) (number->string n))
    ))

(define (kw s)
  (markup (lambda (x) `(b ,x)) (text s)))

(define (combine x1 x2)
  `(span ,x1 ,x2))

(define (program->html p)
  `(pre ,(pretty-markup (program->doc p) combine)))

;; PARSING
(require (rename-in parsack (string pstring) (char pchar)))

(define ws (many (oneOf "\n \t\r"))) ; whitespace

(define (token s)
  (try (parser-compose
        ws
        (x <- (pstring s))
        ws
        (return x))))

(define KEYWORDS (list "in" "let" "zero?" "if" "then" "else"))

(define psymbol
  (try (parser-compose
   ws
   (x <- $identifier)
   (if (member (apply string x) KEYWORDS) $err ws)
   (return (string->symbol (apply string x))))))


(define pnumber
  (try (parser-compose
   ws
   (sign <- (<any> (>> (pchar #\-) (return #t)) (return #f)))
   (n <- (many1 $digit))
   (return (let [(v (string->number (apply string n)))] (if sign (- v) v) )))))
  
(define exp-parser
  (<any>
   (parser-compose (n <- pnumber)
                   (return (const-exp n)))
   (try (parser-compose (token "(")
                        (e1 <- exp-parser)
                        (token "-")
                        (e2 <- exp-parser)
                        (token ")")
                        (return (diff-exp e1 e2))))
   (parser-compose (token "if")
                   (e1 <- exp-parser)
                   (token "then")
                   (e2 <- exp-parser)
                   (token "else")
                   (e3 <- exp-parser)
                   (return (if-exp e1 e2 e3)))
   (parser-compose (token "zero?")
                   (token "(")
                   (e <- exp-parser)
                   (token ")")
                   (return (zero?-exp e)))
   (parser-compose (token "let")
                   (v <- psymbol)
                   (token "=")
                   (e1 <- exp-parser)
                   (token "in")
                   (e2 <- exp-parser)
                   (return (let-exp v e1 e2)))
   (parser-compose (v <- psymbol)
                   (return (var-exp v)))
   )
  )

(define (string->program s)
  (a-program (parse-result exp-parser s)))

;;; Example programs
(require "LET-examples.rkt")
(define examples
  (map (compose string->program second) examples-with-notes))

(define (p n) (list-ref examples n))

;; Web server for playing with programs
(require web-server/servlet
         web-server/servlet-env)

(define-values (dispatch to-url)
    (dispatch-rules
     [("") list-programs]
     [("edit") edit-program]
     [else list-programs]))

(define (list-programs req)
  (response/xexpr
   `(html
       (head
        (title "LET Programming Language")
        (link ((rel "stylesheet") (href "https://www.w3schools.com/w3css/4/w3.css"))))
       (body
        (div ((class "w3-container w3-responsive"))    
             (table ((class "w3-table-all"))
                    (tr (th ((width "20%")) "Description") (th ((width "50%")) "Program") (th ((width "20%")) "Result") (th ((width "10%")) "Edit"))
                    ,@(map (λ (p) `(tr
                                    (td ,(first p))
                                    (td (pre ,(program->html (string->program (second p)))))
                                    (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->string (value-of-program (string->program (second p)))) ))
                                    (td (form ((action ,(to-url edit-program)))
                                              (input ((type "hidden") (name "prog") (value ,(second p))))
                                              (input ((type "submit") (value "Edit"))))
                                     )
                                    )) examples-with-notes )
                    
                    ))))
   )
  )
(define (edit-program req)
  (let* ((p-string (extract-binding/single 'prog (request-bindings req)))
        (p (string->program p-string)))
  (response/xexpr
   `(html
     (head
      (title "LET Programming Language")
      (link ((rel "stylesheet") (href "https://www.w3schools.com/w3css/4/w3.css"))))
     (body
      (div ((class "w3-container w3-responsive"))
           (p (a ((href ,(to-url list-programs))) "Back to list"))
           (table ((class "w3-table-all"))
                  (tr (th "Editable source"))
                  (tr (td (form 
              ((action ,(to-url edit-program))
               (class "w3-container"))
              (textarea ((class "w3-input w3-border") (rows "7") (name "prog")) ,p-string)
              (input ((type "submit"))))
                  (tr (th "Source Program") )
                  (tr (td (pre ,(program->html p))))
                  (tr (th "Result") )
                  (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->string (value-of-program p)) )))
                                  ) 
                  ))))
   ))))

(define (web) (serve/dispatch dispatch))
