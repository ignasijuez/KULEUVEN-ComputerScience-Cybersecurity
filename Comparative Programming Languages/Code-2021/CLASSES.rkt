#lang racket
(require eopl)
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
   (a-program
    (class-decls (list-of class-decl?))
    (body expression?)))

(define-datatype class-decl class-decl?
  (a-class-decl  
    (class-name symbol?)
    (super-name symbol?)
    (field-names (list-of symbol?))
    (method-decls (list-of method-decl?))))

(define-datatype method-decl method-decl?
   (a-method-decl
    (method-name symbol?)
    (vars (list-of symbol?))
    (body expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (zero?-exp (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp (var symbol?))
  (let-exp ; multi-declaration let
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body expression?))
  (proc-exp ; proc with multiple params
   (vars (list-of symbol?))
   (body expression?))  
  (call-exp
   (rator expression?)
   (rand (list-of expression?)))
  (letrec-exp ; letrec with multiple params
   (p-name (list-of symbol?))
   (b-vars (list-of (list-of symbol?)))
   (p-body (list-of expression?))
   (letrec-body expression?))
  (begin-exp (exp1 expression?) (exps (list-of expression?)))
  (assign-exp
   (var symbol?)
   (exp expression?))  
  (sum-exp (exp1 expression?) (exp2 expression?))
  (list-exp (exp (list-of expression?)))
  (new-object-exp
   (class-name symbol?)
   (rands (list-of expression?)))
  (self-exp)
  (method-call-exp
   (obj-exp expression?)
   (method-name symbol?)
   (rands (list-of expression?)))
  (super-call-exp
   (method-name symbol?)
   (rands (list-of expression?))))


;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

;;; world's dumbest model of the store:  the store is a list and a
;;; reference is number which denotes a position in the list.

;; the-store: a Scheme variable containing the current state of the
;; store.  Initially set to a dummy variable.
(define the-store 'uninitialized)

;; empty-store : () -> Sto
;; Page: 111
(define empty-store
  (lambda () '()))

;; initialize-store! : () -> Sto
;; usage: (initialize-store!) sets the-store to the empty-store
;; Page 111
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; get-store : () -> Sto
;; Page: 111
;; This is obsolete.  Replaced by get-store-as-list below
(define get-store
  (lambda () the-store))

;; reference? : SchemeVal -> Bool
;; Page: 111
(define reference?
  (lambda (v)
    (integer? v)))

;; newref : ExpVal -> Ref
;; Page: 111
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store
            (append the-store (list val)))
      next-ref)))                     

;; deref : Ref -> ExpVal
;; Page 111
(define deref 
  (lambda (ref)
    (list-ref the-store ref)))

;; setref! : Ref * ExpVal -> Unspecified
;; Page: 112
(define setref!                       
  (lambda (ref val)
    (set! the-store
          (letrec
              ((setref-inner
                ;; returns a list like store1, except that position ref1
                ;; contains val. 
                (lambda (store1 ref1)
                  (cond
                    ((null? store1)
                     (report-invalid-reference ref the-store))
                    ((zero? ref1)
                     (cons val (cdr store1)))
                    (else
                     (cons
                      (car store1)
                      (setref-inner
                       (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

(define report-invalid-reference
  (lambda (ref the-store)
    (eopl:error 'setref
                "illegal reference ~s in store ~s"
                ref the-store)))

;; get-store-as-list : () -> Listof(List(Ref,Expval))
;; Exports the current state of the store as a scheme list.
;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
;;   where foo, bar, and baz are expvals.
;; If the store were represented in a different way, this would be
;; replaced by something cleverer.
;; Replaces get-store (p. 111)
(define get-store-as-list
  (lambda ()
    (letrec
        ((inner-loop
          ;; convert sto to list as if its car was location n
          (lambda (sto n)
            (if (null? sto)
                '()
                (cons
                 (list n (car sto))
                 (inner-loop (cdr sto) (+ n 1)))))))
      (inner-loop the-store 0))))


;;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

;; an object consists of a symbol denoting its class, and a list of
;; references representing the managed storage for all the fields. 

(define identifier? symbol?)

(define-datatype object object? 
  (an-object
   (class-name identifier?)
   (fields (list-of reference?))))

;; new-object : ClassName -> Obj
;; Page 340
(define new-object                      
  (lambda (class-name)
    (an-object
     class-name
     (map 
      (lambda (field-name)
        (newref (list 'uninitialized-field field-name)))
      (class->field-names (lookup-class class-name))))))

;;;;;;;;;;;;;;;; methods and method environments ;;;;;;;;;;;;;;;;

(define-datatype method method?
  (a-method
   (vars (list-of symbol?))
   (body expression?)
   (super-name symbol?)
   (field-names (list-of symbol?))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; a method environment looks like ((method-name method) ...)

(define method-environment?
  (list-of 
   (lambda (p)
     (and 
      (pair? p)
      (symbol? (car p))
      (method? (cadr p))))))

;; method-env * id -> (maybe method)
(define assq-method-env
  (lambda (m-env id)
    (cond
      ((assq id m-env) => cadr)
      (else #f))))

;; find-method : Sym * Sym -> Method
;; Page: 345
(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (let ((maybe-pair (assq name m-env)))
        (if (pair? maybe-pair) (cadr maybe-pair)
            (report-method-not-found name))))))

(define report-method-not-found
  (lambda (name)
    (eopl:error 'find-method "unknown method ~s" name)))

;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
;; Page: 345
(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

;; method-decls->method-env :
;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
;; Page: 345
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
         (a-method-decl (method-name vars body)
                        (list method-name
                              (a-method vars body super-name field-names)))))
     m-decls)))

;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

(define-datatype class class?
  (a-class
   (super-name (maybe symbol?))
   (field-names (list-of symbol?))
   (method-env method-environment?)))

;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;; the-class-env will look like ((class-name class) ...)

;; the-class-env : ClassEnv
;; Page: 343
(define the-class-env '())

;; add-to-class-env! : ClassName * Class -> Unspecified
;; Page: 343
(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
          (cons
           (list class-name class)
           the-class-env))))

;; lookup-class : ClassName -> Class
(define lookup-class                    
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair (cadr maybe-pair)
          (report-unknown-class name)))))

(define report-unknown-class
  (lambda (name)
    (eopl:error 'lookup-class "Unknown class ~s" name)))



;; constructing classes

;; initialize-class-env! : Listof(ClassDecl) -> Unspecified
;; Page: 344
(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env 
          (list
           (list 'object (a-class #f '() '()))))
    (for-each initialize-class-decl! c-decls)))

;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (c-name s-name f-names m-decls)
                    (let ((f-names
                           (append-field-names
                            (class->field-names (lookup-class s-name))
                            f-names)))
                      (add-to-class-env!
                       c-name
                       (a-class s-name f-names
                                (merge-method-envs
                                 (class->method-env (lookup-class s-name))
                                 (method-decls->method-env
                                  m-decls s-name f-names)))))))))  

;; exercise:  rewrite this so there's only one set! to the-class-env.

;; append-field-names :  Listof(FieldName) * Listof(FieldName) 
;;                       -> Listof(FieldName)
;; Page: 344
;; like append, except that any super-field that is shadowed by a
;; new-field is replaced by a gensym
(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
       (cons 
        (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields))
            (car super-fields))
        (append-field-names
         (cdr super-fields) new-fields))))))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names method-env)
               super-name))))

(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names method-env)
               field-names))))

(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name  field-names method-env)
               method-env))))


(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
                 class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
                 fields))))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

(define maybe
  (lambda (pred)
    (lambda (v)
      (or (not v) (pred v)))))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (obj-val
   (obj object?))
  (list-val
   (lst (list-of expval?)))
  )

;;; extractors:

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (p) p)
      (else (expval-extractor-error 'proc v)))))

(define expval->obj
  (lambda (v)
    (cases expval v
      (obj-val (o) o)
      (else (expval-extractor-error 'object v)))))

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (l) l)
      (else (expval-extractor-error 'list v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (vars (list-of symbol?))
   (body expression?)
   (env environment?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvars (list-of symbol?))
   (bvals (list-of reference?)) 
   (saved-env environment?))
  (extend-env-rec**
   (proc-names (list-of symbol?))
   (b-varss (list-of (list-of symbol?)))
   (proc-bodies (list-of expression?))
   (saved-env environment?))
  (extend-env-with-self-and-super
   (self object?)
   (super-name symbol?)
   (saved-env environment?)))

;; env->list : Env -> List
;; used for pretty-printing and debugging
(define env->list
  (lambda (env)
    (cases environment env
      (empty-env () '())
      (extend-env (sym val saved-env)
                  (cons
                   (list sym val)
                   (env->list saved-env)))
      (extend-env-rec** (p-names b-varss p-bodies saved-env)
                        (cons
                         (list 'letrec p-names '...)
                         (env->list saved-env)))
      (extend-env-with-self-and-super (self super-name saved-env)
                                      (cons
                                       (list 'self self 'super super-name)
                                       (env->list saved-env))))))

;; expval->printable : ExpVal -> List
;; returns a value like its argument, except procedures get cleaned
;; up with env->list 
(define expval->printable
  (lambda (val)
    (cases expval val
      (proc-val (p)
                (cases proc p
                  (procedure (var body saved-env)
                             (list 'procedure var '... (env->list saved-env)))))
      (else val))))

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> environment

;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.  

(define init-env 
  (lambda ()
    (extend-env1
     'i (newref (num-val 1))
     (extend-env1
      'v (newref (num-val 5))
      (extend-env1
       'x (newref (num-val 10))
       (empty-env))))))

(define extend-env1
  (lambda (id val env)
    (extend-env (list id) (list val) env)))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (bvars bvals saved-env)
                  (cond
                    ((location search-sym bvars)
                     => (lambda (n)
                          (list-ref bvals n)))
                    (else
                     (apply-env saved-env search-sym))))
      (extend-env-rec** (p-names b-varss p-bodies saved-env)
                        (cond 
                          ((location search-sym p-names)
                           => (lambda (n)
                                (newref
                                 (proc-val
                                  (procedure 
                                   (list-ref b-varss n)
                                   (list-ref p-bodies n)
                                   env)))))
                          (else (apply-env saved-env search-sym))))
      (extend-env-with-self-and-super (self super-name saved-env)
                                      (case search-sym
                                        ((%self) self)
                                        ((%super) super-name)
                                        (else (apply-env saved-env search-sym)))))))

;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f is
;; sym is not in syms.  We can specify this as follows:
;; if (memv sym syms)
;;   then (list-ref syms (location sym syms)) = sym
;;   else (location sym syms) = #f
(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n) 
            (+ n 1)))
      (else #f))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

; JUST for experimentation
(define calltrace '())
(define envtrace '())

;; value-of-program : Program -> ExpVal
;; Page: 336
(define value-of-program 
  (lambda (pgm)
    (initialize-store!)
    (set! calltrace '())
    (set! envtrace '())
    (cases program pgm
      (a-program (class-decls body)
                 (initialize-class-env! class-decls)
                 (value-of body (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 336 and 337
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (var) (deref (apply-env env var)))
      
      (diff-exp (exp1 exp2)
                (let ((val1
                       (expval->num
                        (value-of exp1 env)))
                      (val2
                       (expval->num
                        (value-of exp2 env))))
                  (num-val
                   (- val1 val2))))
      
      (sum-exp (exp1 exp2)
               (let ((val1
                      (expval->num
                       (value-of exp1 env)))
                     (val2
                      (expval->num
                       (value-of exp2 env))))
                 (num-val
                  (+ val1 val2))))
      
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))
      
      (if-exp (exp0 exp1 exp2) 
              (if (expval->bool (value-of exp0 env))
                  (value-of exp1 env)
                  (value-of exp2 env)))
      
      (let-exp (vars exps body)       
               
               (let ((new-env 
                      (extend-env 
                       vars
                       (map newref (values-of-exps exps env))
                       env)))
                 
                 (value-of body new-env)))
      
      (proc-exp (bvars body)
                (proc-val
                 (procedure bvars body env)))
      
      (call-exp (rator rands)          
                (let ((proc (expval->proc (value-of rator env)))
                      (args (values-of-exps rands env)))
                  (apply-procedure proc args)))
      
      (letrec-exp (p-names b-varss p-bodies letrec-body)
                  (value-of letrec-body
                            (extend-env-rec** p-names b-varss p-bodies env)))
      
      (begin-exp (exp1 exps)
                 (letrec 
                     ((value-of-begins
                       (lambda (e1 es)
                         (let ((v1 (value-of e1 env)))
                           (if (null? es)
                               v1
                               (value-of-begins (car es) (cdr es)))))))
                   (value-of-begins exp1 exps)))
      
      (assign-exp (x e)
                  (begin
                    (setref!
                     (apply-env env x)
                     (value-of e env))
                    (num-val 27)))
      
      
      (list-exp (exps)
                (list-val
                 (values-of-exps exps env)))
      
      ;; new cases for CLASSES language
      
      (new-object-exp (class-name rands)
                      (let ((args (values-of-exps rands env))
                            (obj (new-object class-name)))
                        (set! calltrace (cons (list class-name 'initialize) calltrace)) ; for experimentation
                        (apply-method
                         (find-method class-name 'initialize)
                         obj
                         args)
                        (obj-val obj)))
      
      (self-exp ()
                (obj-val (apply-env env '%self)))
      
      (method-call-exp (obj-exp method-name rands)
                       (let ((args (values-of-exps rands env))
                             (obj (expval->obj (value-of obj-exp env))))
                         (set! calltrace (cons (list (object->class-name obj) method-name) calltrace)) ; for experimentation
                         (apply-method
                          (find-method (object->class-name obj) method-name)
                          obj
                          args)))
      
      (super-call-exp (method-name rands)
                      (let ((args (values-of-exps rands env))
                            (obj (apply-env env '%self)))
                        (set! calltrace (cons (list (apply-env env '%super) method-name) calltrace)) ; for experimentation
                        (apply-method
                         (find-method (apply-env env '%super) method-name)
                         obj
                         args)))        
      )))

;; apply-procedure : Proc * Listof(ExpVal) -> ExpVal
(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (let ((new-env
                        (extend-env
                         vars
                         (map newref args)
                         saved-env)))
                   
                   (value-of body new-env))))))


;; apply-method : Method * Obj * Listof(ExpVal) -> ExpVal
(define apply-method                    
  (lambda (m self args)
    (cases method m
      (a-method (vars body super-name field-names)
                (let ([m-env (extend-env vars (map newref args)
                                      (extend-env-with-self-and-super
                                       self super-name
                                       (extend-env field-names (object->fields self)
                                                   (empty-env))))])
                (set! envtrace (cons m-env envtrace))      ; just for experimentation
                (value-of body m-env))))))

(define values-of-exps
  (lambda (exps env)
    (map
     (lambda (exp) (value-of exp env))
     exps)))




;;; ======== ANYTHING BELOW THIS LINE IS SUPPORT CODE =========
(require racket/exn)
; for playing with programs in DrRacket
(define (test p)
  (begin
    (display (program->string p))
    (display "evaluates to:\n")
    (display (with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->string (value-of-program p)) ))
    (display "\n\n")
    (display "STORE:\n")
    (display (get-store-as-list))
    (display "\n----------------\n")))

;; PRETTY PRINTING
(require (rename-in pprint (empty ppempty)))

(define (program->doc pgm)
  (cases program pgm
    (a-program (class-decls exp1)
               (v-concat `(
                           ,@(map class-decl->doc class-decls)
                           ,(exp->doc exp1 )
                           ,line)))))

(define (class-decl->doc cls)
  (cases class-decl cls
    (a-class-decl (class-name super-name field-names method-decls)
                  (apply v-append
                         (append
                          (list (hs-append (kw "class")
                                     (text (symbol->string class-name))
                                     (kw "extends")
                                     (text (symbol->string super-name))))
                          (map (lambda (f) (indent 2 (hs-append (kw "field") (text (symbol->string f))))) field-names)
                          (map (λ (md) (indent 2 (method-decl->doc md))) method-decls))
                  )
    )))

(define (method-decl->doc md)
  (cases method-decl md
    (a-method-decl (method-name vars body)
                   (v-append
                    (hs-concat `(
                                 ,(kw "method")
                                 ,(text (symbol->string method-name))
                                 ,(text "(")
                                 ,@(apply-infix (text ",")
                                            (map (λ (v) (text (symbol->string v))) vars))
                                 ,(text ")")))
                    (indent 2 (exp->doc body))))))
  
(define (exp->doc exp)
  (cases expression exp
    (const-exp (num) (text (number->string num)))
    (var-exp (var) (text (symbol->string var)))
    (diff-exp (exp1 exp2)
              (let ((val1 (exp->doc exp1))
                    (val2 (exp->doc exp2)))
                (group (h-append lparen space (align (h-append val1 line (char #\-) space (align (h-append val2 space rparen))))))))

    (sum-exp (exp1 exp2)
              (let ((val1 (exp->doc exp1))
                    (val2 (exp->doc exp2)))
                (hs-append lparen (align val1) (char #\+) (align val2) rparen)))    
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
    (let-exp (vars exps body)       
             (let ((vals1 (map exp->doc exps))
                   (val2 (exp->doc body)))
               (h-append
                (kw "let")
                line
                (indent 2 (v-concat
                           (map (λ (v e) (h-append
                                          (text (symbol->string v))
                                          space
                                          equals
                                          space
                                          (align e))) vars vals1)))
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
                 ;(text (symbol->string var)) TODO -> should handle lists of vars
                 rparen
                 line
                 val))
                 ))))
    (call-exp (rator rands)
                     (hs-concat `(
                                  ,(text "(")
                                  ,(exp->doc rator)
                                  ,(text "(")
                                  ,@(apply-infix (text ",") (map exp->doc rands) )
                                  ,(text ")")
                                  ,(text ")")
                                  )))
    (letrec-exp (p-names b-vars p-bodies letrec-body)
                (let ((p-vals (map exp->doc p-bodies))
                      (letrec-val (exp->doc letrec-body)))
                  (h-append
                   (kw "letrec") line
                   space space
                   (align (apply v-append
                                 (map (λ (p-name b-var p-val) (hs-concat `(
                                                               ,(text (symbol->string p-name))
                                                               ,lparen
                                                               ,@(apply-infix (text ",")
                                                                              (map (λ (v) (text (symbol->string v))) b-var))
                                        
                                                               ;(text (symbol->string b-var))
                                                               ,rparen
                                                               ,space
                                                               ,equals
                                                               ,space
                                                               ,(align p-val)
                                        
                                                               ))
                                        ) p-names b-vars p-vals)))
                   line
                   (kw "in")
                   space
                   (align letrec-val)) ))
    (begin-exp (exp1 exps)
               (h-append
                (kw "begin") line
                space space (align
                             (apply v-append (map (λ (e) (h-append (exp->doc e) (char #\;) )) (cons exp1 exps))))
                line
                (kw "end")
                line))
                
    (assign-exp (var exp)
                (let ((val (exp->doc exp)))
                (hs-append (kw "set") (text (symbol->string var)) (char #\=) (align val) )))
    (new-object-exp (class-name rands)
                    (hs-concat `(
                                 ,(kw "new")
                                 ,(text (symbol->string class-name))
                                 ,(text "(")
                                 ,@(apply-infix (text ",")
                                            (map exp->doc rands))
                                 ,(text ")")))
              )
    (list-exp (exps)
              (hs-concat `(
                           ,lbracket
                           ,@(apply-infix comma (map exp->doc exps))
                           ,rbracket))
                     )
    (self-exp () (kw "self"))
    (method-call-exp (obj-exp method-name rands)
                     (hs-concat `(
                                  ,(kw "send")
                                  ,(exp->doc obj-exp)
                                  ,(text (symbol->string method-name))
                                  ,(text "(")
                                  ,@(apply-infix (text ",") (map exp->doc rands) )
                                  ,(text ")"))))
    (super-call-exp (method-name rands)
                    (hs-concat `(
                                  ,(kw "super")
                                  ,(text (symbol->string method-name))
                                  ,(text "(")
                                  ,@(apply-infix (text ",") (map exp->doc rands) )
                                  ,(text ")"))))    
    ))

(define (program->string p)
  (pretty-format (program->doc p)))

(define (expval->string v)
  (pretty-format (expval->doc v)))

(define (env->html env)
  (define (iter env)
    (cases environment env
      (empty-env () `())
      (extend-env (bvars bvals saved-env)
                  (append 
                   (map (λ (bvar bval)
                          `(tr (td ,(symbol->string bvar)) (td ,(string-append "@" (number->string bval))))) (append bvars (make-list (- (length bvals) (length bvars)) '-)) bvals)
                        (iter saved-env)))            
      (extend-env-rec** (p-names b-varss p-bodies saved-env)
                    (append
                     (map (λ (p-name b-vars p-body) `(tr (td ,(string-append (symbol->string p-name) "(" (string-join (map symbol->string b-vars) ",") ")")) (td ,(exp->html p-body)))) p-names b-varss p-bodies)
                        (iter saved-env))
                    )
      (extend-env-with-self-and-super (self super-name saved-env)
                                      (cons
                                       `(tr (td "%self") (td ,(expval->html (obj-val self))))
                                       (cons
                                        `(tr (td "%super") (td (text ,(symbol->string super-name))))
                                        (iter saved-env))))
      ))
  (cons 'table (cons `((class "w3-border")) (iter env)))
    )

(define (obj-val->doc obj)
  (cases object obj
    (an-object (cname fields)
               (h-concat `(
                          ,(kw (symbol->string cname))
                          ,lparen
                          ,@(map (λ (f) (text (string-append "@" (number->string f) " "))) fields)
                          ,rparen)))))
  
(define (expval->doc v)
  (if (expval? v) ; to handle printing uninitialized memory
      (cases expval v
        (bool-val (bool) (text (if bool "#t" "#f")))
        (num-val (n) (text (number->string n)))
        (proc-val (p) (cases proc p
                        (procedure (var body saved-env)
                                   (addenv saved-env ; only add env when converting to HTML
                                           (exp->doc (proc-exp var body))))))
        (obj-val (o) (obj-val->doc o))
        (list-val (vs) (hs-concat `(
                                    ,lbracket
                                    ,@(apply-infix comma (map expval->doc vs))
                                    ,rbracket))) 
        )
      (text "UNINITIALIZED")
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

(define KEYWORDS (list "in" "let" "zero?" "if" "then" "else" "proc" "letrec" "begin" "end" "set" "new" "send" "self" "super" "class" "method" "extends"))

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




(define field-parser
  (parser-compose (token "field")
                  (fname <- psymbol)
                  (return fname)))


                  
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
                        (token "+")
                        (e2 <- exp-parser)
                        (token ")")
                        (return (sum-exp e1 e2))))
   (try (parser-compose (token "set")
                        (v <- psymbol)
                        (token "=")
                        (e <- exp-parser)
                        (return (assign-exp v e))))
   (try (parser-compose (token "(")
                        (e1 <- exp-parser)
                        ws
                        (token "(")
                        (rands <- (sepBy exp-parser (token ",")))
                        (token ")")
                        (token ")")
                        (return (call-exp e1 rands))))
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
                   (l <- (many1 (parser-compose
                                (p-name <- psymbol)
                                (token "(")
                                (v-names <- (sepBy psymbol (token ",")))
                                (token ")")
                                (token "=")
                                (p-body <- exp-parser)
                                (return (list p-name v-names p-body)))))
                   (token "in")
                   (body <- exp-parser)
                   (return (letrec-exp (map first l) (map second l) (map third l) body)))
   (try (parser-compose (token "begin")
                        (l <- (many1 (parser-compose (e <- exp-parser) (token ";") (return e))))
                        (token "end")
                        (return (begin-exp (first l) (cdr l) ))) )
   (parser-compose (token "let")
                   (l <- (many1 (parser-compose
                                 (v <- psymbol)
                                 (token "=")
                                 (e1 <- exp-parser)
                                 (return (list v e1)))))
                   (token "in")
                   (e2 <- exp-parser)
                   (return (let-exp (map first l) (map second l) e2)))
   (parser-compose (token "proc")
                   (token "(")
                   (vs <- (sepBy psymbol (token ",")))
                   (token ")")
                   (body <- exp-parser)
                   (return (proc-exp vs body)))
   (parser-compose (token "new")
                   (cname <- psymbol)
                   (token "(")
                   (rands <- (sepBy exp-parser (token ",")))
                   (token ")")
                   (return (new-object-exp cname rands)))
   (parser-compose (token "send")
                   (obj <- exp-parser)
                   (mname <- psymbol)
                   (token "(")
                   (rands <- (sepBy exp-parser (token ",")))
                   (token ")")
                   (return (method-call-exp obj mname rands)))
   (parser-compose (token "super")
                   (mname <- psymbol)
                   (token "(")
                   (rands <- (sepBy exp-parser (token ",")))
                   (token ")")
                   (return (super-call-exp mname rands)))
   (parser-compose (token "self")
                   (return (self-exp)))
   (parser-compose (token "[")
                   (es <- (sepBy exp-parser (token ",")))
                   (token "]")
                   (return (list-exp es)))
   (parser-compose (v <- psymbol)
                   (return (var-exp v)))
   )
  )

(define mdecl-parser
  (parser-compose (token "method")
                  (mname <- psymbol)
                  (token "(")
                  (vars <- (sepBy psymbol (token ",")))
                  (token ")")
                  (body <- exp-parser)
                  (return (a-method-decl mname vars body))))

(define cdecl-parser
  (parser-compose (token "class")
                  (cname <- psymbol)
                  (token "extends")
                  (sname <- psymbol)
                  (fnames <- (many field-parser))
                  (mdecls <- (many mdecl-parser))
                  (return (a-class-decl cname sname fnames mdecls))))

(define program-parser
  (parser-compose (cdecls <- (many cdecl-parser))
                  (mainexp <- exp-parser)
                  (return (a-program cdecls mainexp))))

(define (string->program s)
  (parse-result (parser-compose (p <- program-parser) ws $eof (return p)) s))

;;; Example programs
(require "CLASSES-examples.rkt")
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
        (title "LETREC Programming Language")
        (link ((rel "stylesheet") (href "https://www.w3schools.com/w3css/4/w3.css"))))
       (body
        (div ((class "w3-container w3-responsive"))    
             (table ((class "w3-table-all"))
                    (tr (th "Description") (th "Program") (th "Result") (th "Store") (th "Edit"))
                    ,@(map (λ (p) `(tr
                                    (td ,(first p))
                                    (td (pre ,(program->html (string->program (second p)))))
                                    (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->html (value-of-program (string->program (second p)))) ))
                                    (td (ul ((class "w3-card-4")) ,@(map (λ (cell) `(li ,(string-append (number->string (first cell)) ":") ,(expval->html (second cell)))) (get-store-as-list))))
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
      (title "LETREC Programming Language")
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
              (input ((type "submit"))))))
                  (tr (th "Source Program") )
                  (tr (td (pre ,(program->html p))))
                  (tr (th "Result") )
                  (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->html (value-of-program p)) )))
                  (tr (th "Call Trace"))
                  (tr (td (ul ((class "w3-card-4")) ,@(map (λ (call env) `(li ,(string-append (symbol->string (first call)) ":" (symbol->string (second call)) ":") ,(env->html env))) (reverse calltrace) (reverse envtrace)))))
                  (tr (th "Store") )
                  (tr (td (ul ((class "w3-card-4")) ,@(map (λ (cell) `(li ,(string-append (number->string (first cell)) ":") ,(expval->html (second cell)))) (get-store-as-list)))))
                  ))))
   )))

(define (web) (serve/dispatch dispatch))
