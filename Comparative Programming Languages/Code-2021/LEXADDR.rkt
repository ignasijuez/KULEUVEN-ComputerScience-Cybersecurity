#lang racket
(require eopl)

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
  (nameless-var-exp (num number?))
  (nameless-let-exp (exp1 expression?) (body expression?))
  (nameless-proc-exp (body expression?))
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
;; procedure : Exp * Nameless-env -> Proc
(define-datatype proc proc?
  (procedure
   ;; in LEXADDR, bound variables are replaced by %nameless-vars, so
   ;; there is no need to declare bound variables.
   ;; (bvar symbol?)
   (body expression?)
   ;; and the closure contains a nameless environment
   (env nameless-environment?)))

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
;; nameless-environment? : SchemeVal -> Bool
;; Page: 99
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

;; empty-nameless-env : () -> Nameless-env
;; Page: 99
(define empty-nameless-env
  (lambda ()
    '()))

;; empty-nameless-env? : Nameless-env -> Bool
(define empty-nameless-env? 
  (lambda (x)
    (null? x)))

;; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env
;; Page: 99
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal
;; Page: 99
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-nameless-env : () -> Nameless-env

(define init-nameless-env
  (lambda ()
    (extend-nameless-env 
     (num-val 1)			; was i
     (extend-nameless-env
      (num-val 5)			; was v
      (extend-nameless-env
       (num-val 10)			; was x
       (empty-nameless-env))))))

;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

;; translation-of-program : Program -> Nameless-program
;; Page: 96
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program                    
                  (translation-of exp1 (init-senv)))))))

;; translation-of : Exp * Senv -> Nameless-exp
;; Page 97
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) (const-exp num))
      (diff-exp (exp1 exp2)
                (diff-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)))
      (zero?-exp (exp1)
                 (zero?-exp
                  (translation-of exp1 senv)))
      (if-exp (exp1 exp2 exp3)
              (if-exp
               (translation-of exp1 senv)
               (translation-of exp2 senv)
               (translation-of exp3 senv)))
      (var-exp (var)
               (nameless-var-exp
                (apply-senv senv var)))
      (let-exp (var exp1 body)
               (nameless-let-exp
                (translation-of exp1 senv)            
                (translation-of body
                                (extend-senv var senv))))
      (proc-exp (var body)
                (nameless-proc-exp
                 (translation-of body
                                 (extend-senv var senv))))
      (call-exp (rator rand)
                (call-exp
                 (translation-of rator senv)
                 (translation-of rand senv)))
      (else (report-invalid-source-expression exp))
      )))

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error 'value-of 
                "Illegal expression in source code: ~s" exp)))

;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

;;; Senv = Listof(Sym)
;;; Lexaddr = N

;; empty-senv : () -> Senv
;; Page: 95
(define empty-senv
  (lambda ()
    '()))

;; extend-senv : Var * Senv -> Senv
;; Page: 95
(define extend-senv
  (lambda (var senv)
    (cons var senv)))

;; apply-senv : Senv * Var -> Lexaddr
;; Page: 95
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((eqv? var (car senv))
       0)
      (else
       (+ 1 (apply-senv (cdr senv) var))))))

(define report-unbound-var
  (lambda (var)
    (eopl:error 'translation-of "unbound variable in code: ~s" var)))

;; init-senv : () -> Senv
;; Page: 96
(define init-senv
  (lambda ()
    (extend-senv 'i
                 (extend-senv 'v
                              (extend-senv 'x
                                           (empty-senv))))))



;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-translation : Nameless-program -> ExpVal
;; Page: 100
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-nameless-env))))))
  
;; value-of : Nameless-exp * Nameless-env -> ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))

      (diff-exp (exp1 exp2)
                (let ((val1
                       (expval->num
                        (value-of exp1 nameless-env)))
                      (val2
                       (expval->num
                        (value-of exp2 nameless-env))))
                  (num-val
                   (- val1 val2))))
        
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 nameless-env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))

      (if-exp (exp0 exp1 exp2) 
              (if (expval->bool (value-of exp0 nameless-env))
                  (value-of exp1 nameless-env)
                  (value-of exp2 nameless-env)))

      (call-exp (rator rand)          
                (let ((proc (expval->proc (value-of rator nameless-env)))
                      (arg (value-of rand nameless-env)))
                  (apply-procedure proc arg)))

      (nameless-var-exp (n)
                        (apply-nameless-env nameless-env n))

      (nameless-let-exp (exp1 body)
                        (let ((val (value-of exp1 nameless-env)))
                          (value-of body
                                    (extend-nameless-env val nameless-env))))

      (nameless-proc-exp (body)
                         (proc-val
                          (procedure body nameless-env)))

      (else
       (eopl:error 'value-of 
                   "Illegal expression in translated code: ~s" exp))

      )))


;; apply-procedure : Proc * ExpVal -> ExpVal

(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (body saved-env)
                 (value-of body (extend-nameless-env arg saved-env))))))

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
    (nameless-var-exp (num) (text (string-append "%" (number->string num))))
    (nameless-let-exp (exp body) (let ((val1 (exp->doc exp))
                                       (val2 (exp->doc body)))
                                   (h-append
                                    (kw "%let")
                                    space
                                    (align val1)
                                    line
                                    (kw "in")
                                    space
                                    (align val2)) ))
    (nameless-proc-exp (body) (let ((val (exp->doc body)))
                                (h-append
                                 (kw "%lexproc")
                                 space
                                 (align val)
                                 )))
   ))

(define (program->string p)
  (pretty-format (program->doc p)))

(define (env->string env) ;; TODO: INCOMPLETE HACK
  (string-join (map expval->string env) ", " #:before-first "[" #:after-last "]"))

(define (expval->string v)
  (cases expval v
    (bool-val (bool) (if bool "#t" "#f"))
    (num-val (n) (number->string n))
    (proc-val (p) (cases proc p
                    (procedure (body saved-env)
                               (string-append "%lexproc "  (pretty-format (exp->doc body)) (env->string saved-env) ))))
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

(define KEYWORDS (list "in" "let" "zero?" "if" "then" "else" "proc"))

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

;; DRAWING OF CONTOUR DIAGRAMS

; draw x y gives as result an svg xexpr drawing the pic at coordinates x y
(struct pic (width height draw))


; render top bottoms draws a tree with top-node top, bottom nodes in the
; (non-empty) list bottoms
(define (render top nodes)
  (define delta 20) ; delta between bottom nodes, and between top node and bottoms
  (let* ([nts number->string]
         [w (max
            (pic-width top)
            (+ (apply + (map pic-width nodes))
               (* (- (length nodes) 1) delta)))] ; width of the rendered tree
         [ht (pic-height top)] ; height of the top-node
         [ht+delta (+ ht delta)] 
         [h (+ ht+delta (apply max (map pic-height nodes)))] ; height of the rendered tree
         [w2 (/ w 2)])
    (pic w h
         (λ (dx dy)
           `(svg ((x ,(nts dx))
                  (y ,(nts dy))
                  (width ,(nts w))
                  (height ,(nts h)))
                 ;; top node
                 ,((pic-draw top) (/ (- w (pic-width top)) 2) 0)
                 ;; bottom nodes and lines
                 ,@(let-values ([(bns lns x) ; bottom nodes, lines, x-coord of bottom node
                     (for/fold ([ns '()]
                                [ls '()]
                                [x 0])
                               ([n nodes])
                       (values (cons ((pic-draw n) x ht+delta) ns)
                               (cons `(line ((x1 ,(nts w2))
                                             (y1 ,(nts ht))
                                             (x2 ,(nts (+ x (/ (pic-width n) 2))))
                                             (y2 ,(nts ht+delta))
                                             (stroke "black"))) ls)
                               (+ x (+ (pic-width n) delta))))])
                      (append bns lns))
                 )))
                 
    ))

(define (bgcolour c p) ; put a transparent color c on pic p
  (pic (pic-width p) (pic-height p)
       (λ (dx dy)
         `(svg ((x ,(number->string dx))
                (y ,(number->string dy))
                (width ,(number->string (pic-width p)))
                (height ,(number->string (pic-height p))))
               ,((pic-draw p) 0 0)
               (rect ((x "0")
                      (y "0")
                      (width ,(number->string (pic-width p)))
                      (height ,(number->string (pic-height p)))
                      (fill ,c)
                      (fill-opacity "0.2")
                      (stroke "none")
                      (rx "10")
                      (ry "10")))
               ))))

(define (t s) ; make a pic with some text
  (pic (* (string-length s) 10) 24 ; hack - don't know how to compute size of rendered string
       (λ (dx dy)
         `(text ((font-family "courier")
                 (font-size "16")
                 (x ,(number->string dx))
                 (y ,(number->string (+ dy 20)))
                 (fill "black"))
                ,s))))


(define (draw-exp exp) ; draw expression as a pic
  (cases expression exp
    (const-exp (num) (t (number->string num)))
    (var-exp (var) (t (symbol->string var)))
    (nameless-var-exp (num) (t (string-append "%" (number->string num))))
    (diff-exp (exp1 exp2)
              (render (t "-") (list (draw-exp exp1) (draw-exp exp2))))
    (zero?-exp (exp1)
               (render (t "zero?") (list (draw-exp exp1))))
    (if-exp (exp1 exp2 exp3)
            (render (t "if") (list (draw-exp exp1) (draw-exp exp2) (draw-exp exp3))))
    (let-exp (var exp1 body)       
             (render (t (string-append "let " (symbol->string var))) (list (draw-exp exp1) (bgcolour "red" (draw-exp body)))))
    (nameless-let-exp (exp1 body)       
                      (render (t "let%") (list (draw-exp exp1) (bgcolour "red" (draw-exp body)))))
    (proc-exp (var body)
              (render (t (string-append "proc(" (symbol->string var) ")")) (list (bgcolour "blue" (draw-exp body)) )))
    (nameless-proc-exp (body)
                       (render (t "proc%") (list (bgcolour "blue" (draw-exp body)) )))      
    (call-exp (rator rand)
              (render (t "app") (list (draw-exp rator) (draw-exp rand))))
    ))

(define (draw-program p) ; draw program as a pic
  (cases program p
    (a-program (exp1)
               (draw-exp exp1))))

(define (draw pgm) ; generate svg xexpr for a program
  (let ([p (draw-program pgm)])
   `(svg ((width ,(number->string (pic-width p))) (height ,(number->string (pic-height p))))
             ,((pic-draw p) 0 0))
    ))

;;; Example programs
(require "PROC-examples.rkt")
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
        (title "LEXADDR Programming Language")
        (link ((rel "stylesheet") (href "https://www.w3schools.com/w3css/4/w3.css"))))
       (body
        (div ((class "w3-container w3-responsive"))    
             (table ((class "w3-table-all"))
                    (tr (th "Source Program") (th "Target Program") (th "Edit"))
                    ,@(map (λ (p) `(tr
                                    (td (pre ,(program->html p)) (br) ,(draw p) )
                                    (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (program->html (translation-of-program p)) )
                                        (br)
                                        ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (draw (translation-of-program p))))
                                    (td (form ((action ,(to-url edit-program)))
                                              (input ((type "hidden") (name "prog") (value ,(program->string p))))
                                              (input ((type "submit") (value "Edit"))))
                                     )
                                    )) (map (compose string->program second) examples-with-notes) )
                    
                    ))))
   )
  )
(define (edit-program req)
  (let* ((p-string (extract-binding/single 'prog (request-bindings req)))
        (p (string->program p-string)))
  (response/xexpr
   `(html
     (head
      (title "LEXADDR Programming Language")
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
                  (tr (th "Target Program") )
                  (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (program->html (translation-of-program p)) )))
                  (tr (th "Result of Target Program") )
                  (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->string (value-of-program (translation-of-program p))) )))
                  (tr (th "Source Contour Diagram") )
                  (tr (td ,(draw p)))
                  (tr (th "Target Contour Diagram") )
                  (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (draw (translation-of-program p)))))
                  ))))
   )))))

(define (web) (serve/dispatch dispatch))
