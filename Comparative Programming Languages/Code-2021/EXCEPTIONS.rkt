#lang racket
(require eopl)
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
  (a-program (exp1 expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (unop-exp (oper unary-op?) (exp1 expression?)) ; Unary operators
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  (proc-exp
   (var symbol?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (p-name symbol?)
   (b-var symbol?)
   (p-body expression?)
   (letrec-body expression?))
  (const-list-exp (nums (list-of number?)))
  (try-exp
   (body expression?)
   (var symbol?)
   (handler expression?))
  (raise-exp (exp1 expression?)))
  
(define-datatype
   unary-op
   unary-op?
   (null?-unop)
   (car-unop)
   (cdr-unop)
   (zero?-unop))

(define (oper->string oper)
  (cases unary-op oper
    (null?-unop () "null?")
   (car-unop () "car")
   (cdr-unop () "cdr")
   (zero?-unop () "zero?")))


;; Semantics
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (list-val
   (lst (list-of expval?))))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (env environment?))
  )

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (p) p)
      (else (expval-extractor-error 'proc v)))))

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (else (expval-extractor-error 'list v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; Environments
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (id symbol?)
   (bvar symbol?)
   (body expression?)
   (saved-env environment?)))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (bvar bval saved-env)
                  (if (eqv? search-sym bvar)
                      bval
                      (apply-env saved-env search-sym)))
      (extend-env-rec (p-name b-var p-body saved-env)
                      (if (eqv? search-sym p-name)
                          (proc-val (procedure b-var p-body env))          
                          (apply-env saved-env search-sym))))))

;; init-env : () -> Env
(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;; Continuations


(define-datatype continuation continuation?
  (end-cont)                          ; []
  (diff1-cont                       
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont                         
   (val1 expval?)
   (cont continuation?))
  (unop-arg-cont
   (unop unary-op?)
   (cont continuation?))
  (let-exp-cont
   (var symbol?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (rator-cont            
   (rand expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont                          
   (val1 expval?)
   (cont continuation?))
  (try-cont
   (var symbol?)
   (handler-exp expression?)
   (env environment?)
   (cont continuation?))
  (raise1-cont
   (saved-cont continuation?))
  )

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
; ONLY FOR VISUALIZATION
(define trace '())

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    ; ONLY FOR VISUALIZATION
    (set! trace '())    
    (cases program pgm
      (a-program (body)
                 (value-of/k body (init-env) (end-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 173
(define value-of/k
  (lambda (exp env cont)
    ; ONLY FOR VISUALIZATION
    (set! trace (cons (list 'VOK exp env "" cont) trace))    
    (cases expression exp
      
      (const-exp (num) (apply-cont cont (num-val num)))
      
      (const-list-exp (nums)
                      (apply-cont cont
                                  (list-val (map num-val nums))))
      
      (var-exp (var) (apply-cont cont (apply-env env var)))
      
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env
                            (diff1-cont exp2 env cont)))
      
      (unop-exp (unop exp1)
                (value-of/k exp1 env
                            (unop-arg-cont unop cont)))
      
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-test-cont exp2 exp3 env cont)))
      
      (proc-exp (var body)
                (apply-cont cont
                            (proc-val
                             (procedure var body env))))
      
      (call-exp (rator rand)
                (value-of/k rator env
                            (rator-cont rand env cont)))
      

      (let-exp (var exp1 body)
               (value-of/k exp1 env
                           (let-exp-cont var body env cont)))
      
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of/k
                   letrec-body
                   (extend-env-rec p-name b-var p-body env)
                   cont))
      
      (try-exp (exp1 var handler-exp)
               (value-of/k exp1 env
                           (try-cont var handler-exp env cont)))
      
      (raise-exp (exp1)
                 (value-of/k exp1 env
                             (raise1-cont cont))))))

;; apply-cont : continuation * expval -> final-expval

(define apply-cont
  (lambda (cont val)
    ; ONLY FOR VISUALIZATION
    (set! trace (cons (list 'AC "" "" val cont) trace))    
    (cases continuation cont
      (end-cont () val)
      (diff1-cont (exp2 saved-env saved-cont)
                  (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
      (diff2-cont (val1 saved-cont)
                  (let ((n1 (expval->num val1))
                        (n2 (expval->num val)))
                    (apply-cont saved-cont
                                (num-val (- n1 n2)))))
      (unop-arg-cont (unop cont)
                     (apply-cont cont
                                 (apply-unop unop val)))
      (let-exp-cont (var body saved-env saved-cont)
                    (value-of/k body
                                (extend-env var val saved-env) saved-cont))
      (if-test-cont (exp2 exp3 env cont)
                    (if (expval->bool val)
                        (value-of/k exp2 env cont)
                        (value-of/k exp3 env cont)))
      (rator-cont (rand saved-env saved-cont)
                  (value-of/k rand saved-env
                              (rand-cont val saved-cont)))
      (rand-cont (val1 saved-cont)
                 (let ((proc (expval->proc val1)))
                   (apply-procedure proc val saved-cont)))
      ;; the body of the try finished normally-- don't evaluate the handler
      (try-cont (var handler-exp saved-env saved-cont)
                (apply-cont saved-cont val))
      ;; val is the value of the argument to raise
      (raise1-cont (saved-cont)
                   (apply-handler val saved-cont))
      )))

;; apply-handler : ExpVal * Cont -> FinalAnswer
(define apply-handler
  (lambda (val cont)
    (cases continuation cont
      ;; interesting cases
      (try-cont (var handler-exp saved-env saved-cont)
                (value-of/k handler-exp
                            (extend-env var val saved-env)
                            saved-cont))
      
      (end-cont () (eopl:error 'apply-handler "uncaught exception!"))
      
      ;; otherwise, just look for the handler...
      (diff1-cont (exp2 saved-env saved-cont)
                  (apply-handler val saved-cont))
      (diff2-cont (val1 saved-cont)
                  (apply-handler val saved-cont))
      (if-test-cont (exp2 exp3 env saved-cont)
                    (apply-handler val saved-cont))
      (unop-arg-cont (unop saved-cont)
                     (apply-handler val saved-cont))
      (rator-cont (rand saved-env saved-cont)
                  (apply-handler val saved-cont))
      (rand-cont (val1 saved-cont)
                 (apply-handler val saved-cont))
      (raise1-cont (cont)
                   (apply-handler val cont))
      (let-exp-cont (var body saved-env saved-cont)
                    (apply-handler val saved-cont))
      )))


;; apply-procedure : procedure * expval * cont -> final-expval

(define apply-procedure
  (lambda (proc1 arg cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body
                             (extend-env var arg saved-env)
                             cont)))))

(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
      (null?-unop ()
                  (bool-val
                   (null? (expval->list val))))
      (car-unop ()
                (car (expval->list val)))
      (cdr-unop ()
                (list-val (cdr (expval->list val))))
      (zero?-unop ()
                  (bool-val
                   (zero? (expval->num val)))))))



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
    (unop-exp (oper exp1)
               (let ((val1 (exp->doc exp1 )))
                 (hs-append (kw (oper->string oper)) lparen (align val1) rparen)))
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
    (proc-exp (var body)
              (let ((val (exp->doc body)))
                (group (h-append
                 (kw "proc")
                 (align (h-append
                 lparen
                 (text (symbol->string var))
                 rparen
                 line
                 val))
                 ))))
    (call-exp (rator rand)
              (let ((val1 (exp->doc rator))
                    (val2 (exp->doc rand)))
                (group (h-append lparen space (align (h-append val1 line (align (h-append val2 space rparen))))))))
    (letrec-exp (p-name b-var p-body letrec-body)
                (let ((p-val (exp->doc p-body))
                      (letrec-val (exp->doc letrec-body)))
                  (h-append
                   (kw "letrec")
                   space
                   (text (symbol->string p-name))
                   lparen
                   (text (symbol->string b-var))
                   rparen
                   space
                   equals
                   space
                   (align p-val)
                   line
                   (kw "in")
                   space
                   (align letrec-val)) ))
    (try-exp (body var handler)
            (let ((bval (exp->doc body ))                  
                  (hval (exp->doc handler )))
              (group (h-append
                      (kw "try")
                      space
                      (align bval)
                      line
                      (kw "catch")
                      lparen
                      (text (symbol->string var))
                      rparen                      
                      line
                      (align hval) )))          )
    (const-list-exp (nums)
                    (apply hs-append
                           (append
                           (cons lbracket
                                 (apply-infix comma (map (λ (n) (text (number->string n))) nums)))
                           (list rbracket))
                           ))
    (raise-exp (exp1)
               (let ((val1 (exp->doc exp1 )))
                 (hs-append (kw "raise") (align val1))))
    ))

(define (cont->doc k)
  (define (inner k hole)
    (cases continuation k
      (end-cont () hole)
      (unop-arg-cont (oper k2) 
       (inner k2 (hs-append (kw (oper->string oper)) lparen (align hole) rparen)))
      (let-exp-cont (var body saved-env saved-cont) 
       (inner saved-cont
              (h-append
                (kw "let")
                space
                (text (symbol->string var))
                space
                equals
                space
                (align hole)
                line
                (kw "in")
                space
                (align (exp->doc body)))
        ))
      (if-test-cont (exp2 exp3 saved-env saved-cont) 
       (inner saved-cont
              (group (h-append
                      (kw "if")
                      space
                      (align hole)
                      line
                      (kw "then")
                      space
                      (align (exp->doc exp2))
                      line
                      (kw "else")
                      space
                      (align (exp->doc exp3)) ))))    
      (diff1-cont (exp env k2) 
       (inner k2
              (hs-append lparen (align hole) (char #\-) (align (exp->doc exp)) rparen)
        ))
      (diff2-cont (v1 k2) 
       (inner k2
              (hs-append lparen (align (expval->doc v1)) (char #\-) (align hole) rparen)
        ))
      (rator-cont (rand env k2) 
       (inner k2
              (hs-append lparen (align hole) space (align (exp->doc rand)) rparen)
        ))
      (rand-cont (v1 k2) 
       (inner k2
              (hs-append lparen (align (expval->doc v1)) space (align hole) rparen)
         ))
      (raise1-cont (k2) 
       (inner k2 (hs-append (kw "raise")  (align hole))))
      (try-cont (var handler-exp env cont )
                (inner cont (h-append
                             (kw "try") space hole
                             line
                             (kw "catch") lparen (text (symbol->string var)) rparen (exp->doc handler-exp)
                                                              )))
      )
    )
  (inner k (kw "[ ]")))
    
(define (program->string p)
  (pretty-format (program->doc p)))

(define (expval->string v)
  (pretty-format (expval->doc v)))

(define (env->html env)
  (define (iter env)
    (cases environment env
      (empty-env () `())
      (extend-env (bvar bval saved-env)
                  (cons `(tr (td ,(symbol->string bvar)) (td ,(expval->html bval)))
                        (iter saved-env)))            
      (extend-env-rec (p-name b-var p-body saved-env)
                    (cons `(tr (td ,(string-append (symbol->string p-name) "(" (symbol->string b-var) ")")) (td ,(exp->html p-body)))
                        (iter saved-env))  
                    )))
  (cons 'table (cons `((class "w3-border")) (iter env)))
    )

(define (expval->doc v)
  (cases expval v
    (bool-val (bool) (text (if bool "#t" "#f")))
    (num-val (n) (text (number->string n)))
    (proc-val (p) (cases proc p
                    (procedure (var body saved-env)
                               (addenv saved-env ; only add env when converting to HTML
                                       (exp->doc (proc-exp var body))))))
    (list-val (nums) (apply hs-append (append (list lbracket) (add-between (map (λ (n) (expval->doc n)) nums) comma) (list rbracket))))
    ))

(define (kw s)
  (markup (lambda (x) `(b ,x)) (text s)))

(define (addenv env d)
  (markup (lambda (x) `(table (tr (td (pre ,x)) (td ,(env->html env))))) d))

(define (combine x1 x2)
  `(span ,x1 ,x2))

(define (program->html p)
  `(pre ,(pretty-markup (program->doc p) combine)))

(define (exp->html p)
  `(pre ,(pretty-markup (exp->doc p) combine 20)))

(define (cont->html k)
  `(pre ,(pretty-markup (cont->doc k) combine 20)))

(define (expval->html v)
  (pretty-markup (expval->doc v) combine 20))

;; PARSING
(require (rename-in parsack (string pstring) (char pchar)))

(define ws (many (oneOf "\n \t\r"))) ; whitespace

(define (token s)
  (try (parser-compose
        ws
        (x <- (pstring s))
        ws
        (return x))))

(define KEYWORDS (list "in" "let" "zero?" "null?" "car" "cdr" "if" "then" "else" "proc" "letrec"))

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
                        (token "- "); NOTE space after - : (x-1) is parsed as x applied to -1
                        (e2 <- exp-parser)
                        (token ")")
                        (return (diff-exp e1 e2))))
   (try (parser-compose (token "(")
                        (e1 <- exp-parser)
                        ws
                        (e2 <- exp-parser)
                        (token ")")
                        (return (call-exp e1 e2))))
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
                   (return (unop-exp (zero?-unop) e)))
   (parser-compose (token "null?")
                   (token "(")
                   (e <- exp-parser)
                   (token ")")
                   (return (unop-exp (null?-unop) e)))
   (parser-compose (token "car")
                   (token "(")
                   (e <- exp-parser)
                   (token ")")
                   (return (unop-exp (car-unop) e)))
   (parser-compose (token "cdr")
                   (token "(")
                   (e <- exp-parser)
                   (token ")")
                   (return (unop-exp (cdr-unop) e)))
   (parser-compose (token "letrec")
                   (p-name <- psymbol)
                   (token "(")
                   (v-name <- psymbol)
                   (token ")")
                   (token "=")
                   (p-body <- exp-parser)
                   (token "in")
                   (body <- exp-parser)
                   (return (letrec-exp p-name v-name p-body body)))   
   (parser-compose (token "let")
                   (v <- psymbol)
                   (token "=")
                   (e1 <- exp-parser)
                   (token "in")
                   (e2 <- exp-parser)
                   (return (let-exp v e1 e2)))
   (parser-compose (token "proc")
                   (token "(")
                   (v <- psymbol)
                   (token ")")
                   (body <- exp-parser)
                   (return (proc-exp v body)))
   (parser-compose (token "[")
                   (es <- (sepBy pnumber (token ",")))
                   (token "]")
                   (return (const-list-exp es)))
   (parser-compose (token "raise")
                   (e <- exp-parser)
                   (return (raise-exp e)))
   (parser-compose (token "try")
                   (e1 <- exp-parser)
                   (token "catch")
                   (token "(")
                   (v <- psymbol)
                   (token ")")
                   (e2 <- exp-parser)
                   (return (try-exp e1 v e2)))
   (parser-compose (v <- psymbol)
                   (return (var-exp v)))
   )
  )

(define (string->program s)
  (a-program (parse-result exp-parser s)))

;;; Example programs
(require "EXCEPTIONS-examples.rkt")
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
     [("show" (integer-arg)) show-program]
     [else list-programs]))

(define (list-programs req)
  (response/xexpr
   `(html
       (head
        (title "LETREC-CONT Programming Language")
        (link ((rel "stylesheet") (href "https://www.w3schools.com/w3css/4/w3.css"))))
       (body
        (div ((class "w3-container w3-responsive"))    
             (table ((class "w3-table-all"))
                    (tr (th ((width "20%")) "Description") (th ((width "50%")) "Program") (th ((width "20%")) "Result") (th ((width "10%")) "Edit"))
                    ,@(map (λ (p) `(tr
                                    (td ,(first p))
                                    (td (pre ,(program->html (string->program (second p)))))
                                    (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->html (value-of-program (string->program (second p)))) ))
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
      (title "LETREC-CONT Programming Language")
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
                  (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->html (value-of-program p)) )))
                  (tr (td (form ((action ,(to-url show-program 0)))
                                              (input ((type "hidden") (name "prog") (value ,p-string)))
                                              (input ((type "submit") (value "Show trace without environments"))))
                          (form ((action ,(to-url show-program 1)))
                                (input ((type "hidden") (name "prog") (value ,p-string)))
                                (input ((type "submit") (value "Show trace with environments"))))
                          ))
                                  ) 
                  ))))
   ))))

(define (show-program req env-int)
  (let* ((p-string (extract-binding/single 'prog (request-bindings req)))
        (p (string->program p-string))
        (env? (not (eqv? env-int 0))))
    (response/xexpr
     `(html
       (head
        (title "LETREC-CONT Execution trace")
        (link ((rel "stylesheet") (href "https://www.w3schools.com/w3css/4/w3.css"))))
       (body
        (div ((class "w3-container w3-responsive"))
             (pre ,(program->html p))
             (hr)
             (text ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->html (value-of-program p)) ))
             (hr)
             (table ((class "w3-table-all w3-responsive"))
                    (tr (th "Type") (th "Exp") ,@(if env? `((th "Env")) '()) (th "Val") (th "Cont"))
                    ,@(map (λ (entry)
                             (if (eqv? (first entry) 'AC)
                                 `(tr ((class "w3-khaki")) (td "AC")
                                    (td "-")
                                    ,@(if env? `((td "-")) '())
                                    (td ,(expval->html (fourth entry)))
                                    (td ,(cont->html (fifth entry)) ))
                                 `(tr ((class "w3-gray")) (td "VOK")
                                    (td (pre ,(exp->html (second entry))))
                                    ,@(if env? `((td ,(env->html (third entry)))) '())
                                    (td "-")
                                    (td ,(cont->html (fifth entry)) )))) (reverse trace))
                    )))))))

(define (web) (serve/dispatch dispatch))
