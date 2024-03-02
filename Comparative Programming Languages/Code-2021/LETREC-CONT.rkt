#lang racket
(require eopl)
(provide (all-defined-out))

;;; Syntax
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
  )

;; Semantics
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  )

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

;; Page: 148
(define identifier? symbol?)

(define-datatype continuation continuation?
  (end-cont)                   ; [ ]
  (zero1-cont
   (saved-cont continuation?)) ; E[zero? [] ]
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?)) ; E[let var = [] in body]
  (if-test-cont 
   (exp2 expression?)
   (exp3 expression?)
   (saved-env environment?)
   (saved-cont continuation?)) ; E[if [] then exp1 else exp2]
  (diff1-cont                
   (exp2 expression?)
   (saved-env environment?)
   (saved-cont continuation?)) ; E[ [] - exp2 ]
  (diff2-cont                
   (val1 expval?)
   (saved-cont continuation?)) ; E[ val1 - [] ]
  (rator-cont            
   (rand expression?)
   (saved-env environment?)
   (saved-cont continuation?)) ; E[([] rand)]
  (rand-cont             
   (val1 expval?)
   (saved-cont continuation?))); E[(val1 [])]


;; Interpreter
;; apply-cont : Cont * ExpVal -> FinalAnswer
;; Page: 148
(define apply-cont
  (lambda (cont val)
    ; ONLY FOR VISUALIZATION
    ;(display (string-append "AC : val = " (expval->string val) ", cont = " (cont->string cont) "\n"))
    (set! trace (cons (list 'AC "" "" val cont) trace))

    (cases continuation cont
      (end-cont () 
                (begin
                  (eopl:printf
                   "End of computation.~%")
                  val))
      (zero1-cont (saved-cont)
                  (apply-cont saved-cont
                              (bool-val
                               (zero? (expval->num val)))))
      (let-exp-cont (var body saved-env saved-cont)
                    (value-of/k body
                                (extend-env var val saved-env) saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
                    (if (expval->bool val)
                        (value-of/k exp2 saved-env saved-cont)
                        (value-of/k exp3 saved-env saved-cont)))
      (diff1-cont (exp2 saved-env saved-cont)
                  (value-of/k exp2
                              saved-env (diff2-cont val saved-cont)))
      (diff2-cont (val1 saved-cont)
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val)))
                    (apply-cont saved-cont
                                (num-val (- num1 num2)))))
      (rator-cont (rand saved-env saved-cont)
                  (value-of/k rand saved-env
                              (rand-cont val saved-cont)))
      (rand-cont (val1 saved-cont)
                 (let ((proc (expval->proc val1)))
                   (apply-procedure/k proc val saved-cont)))
      )))


; ONLY FOR VISUALIZATION
(define trace '())

;; value-of-program : Program -> FinalAnswer
;; Page: 143 and 154
(define value-of-program 
  (lambda (pgm)
    ; ONLY FOR VISUALIZATION
    (set! trace '())
    (cases program pgm
      (a-program (exp1)
                 (value-of/k exp1 (init-env) (end-cont))))))  

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 143--146, and 154
(define value-of/k
  (lambda (exp env cont)
    ; ONLY FOR VISUALIZATION
    ;(display (string-append "VOK: exp = " (exp->string exp) " , cont = " (cont->string cont) " )\n" ))
    (set! trace (cons (list 'VOK exp env "" cont) trace))

    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
                (apply-cont cont 
                            (proc-val (procedure var body env))))
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of/k letrec-body
                              (extend-env-rec p-name b-var p-body env)
                              cont))
      (zero?-exp (exp1)
                 (value-of/k exp1 env
                             (zero1-cont cont)))
      (let-exp (var exp1 body)
               (value-of/k exp1 env
                           (let-exp-cont var body env cont)))
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-test-cont exp2 exp3 env cont)))
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env
                            (diff1-cont exp2 env cont)))        
      (call-exp (rator rand) 
                (value-of/k rator env
                            (rator-cont rand env cont)))
      )))


;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body
                             (extend-env var arg saved-env)
                             cont)))))


;;; ======== ANYTHING BELOW THIS LINE IS SUPPORT CODE =========
(require racket/exn)
; for playing with programs in DrRacket
(define (test p)
  (begin
    (display (program->string p))
    (display "evaluates to:\n")
    (display (with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->string (value-of-program p)) ))
    (display "\n+++++ TRACE ++++++++\n")
    (for/list ([e (reverse trace)] #:when (eqv? 'VOK (car e)))
      (display (string-append "VOK( " (exp->string (second e)) ",      " (cont->string (fifth e)) " )\n")))
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
    ))

(define (cont->doc k)
  (define (inner k hole)
    (cases continuation k
      (end-cont () hole)
      (zero1-cont (k2) 
       (inner k2 (hs-append (kw "zero?") lparen (align hole) rparen)))
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
      )
    )
  (inner k (kw "[ ]")))
    
(define (program->string p)
  (pretty-format (program->doc p)))

(define (cont->string p)
  (pretty-format (cont->doc p)))

(define (exp->string p)
  (pretty-format (exp->doc p)))

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
                                       ;(exp->doc (proc-exp var body))
                                       (text "<proc>") ; alternative for short traces
                                       ))))                                                      
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

(define KEYWORDS (list "in" "let" "zero?" "if" "then" "else" "proc" "letrec"))

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
                   (return (zero?-exp e)))
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
   (parser-compose (v <- psymbol)
                   (return (var-exp v)))
   )
  )

(define (string->program s)
  (a-program (parse-result exp-parser s)))

;;; Example programs
(require "LETREC-CONT-examples.rkt")
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
