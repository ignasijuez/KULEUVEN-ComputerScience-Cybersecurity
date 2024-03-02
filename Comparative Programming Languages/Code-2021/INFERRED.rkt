#lang racket
(require eopl)
(provide (all-defined-out))

;;; Syntax
(define-datatype type type?
  (int-type)
  (bool-type)
  (proc-type (arg-type type?) (result-type type?))
  (tvar-type (n number?)))

(define-datatype optional-type optional-type?
  (no-type)
  (a-type (ty type?)))

; predicates on types
(define atomic-type?
  (lambda (ty)
    (cases type ty
      (proc-type (ty1 ty2) #f)
      (tvar-type (sn) #f)
      (else #t))))

(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (t1 t2) #t)
      (else #f))))

(define tvar-type?
  (lambda (ty)
    (cases type ty
      (tvar-type (serial-number) #t)
      (else #f))))

;extractors
(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) arg-type)
      (else (eopl:error 'proc-type->arg-type
                        "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) result-type)
      (else (eopl:error 'proc-type->result-types
                        "Not a proc type: ~s" ty)))))

; PROGRAMS and EXPRESSIONS
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
   (ty optional-type?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (p-result optional-type?)
   (p-name symbol?)
   (b-var symbol?)
   (b-var-type optional-type?)
   (p-body expression?)
   (letrec-body expression?))
  )

(define (type->string ty)
  (cases type ty
    (int-type () "int")
    (bool-type () "bool")
    (proc-type (arg-type result-type ) (string-append "(" (type->string arg-type) " -> " (type->string result-type) ")"))
    (tvar-type (n) (string-append "%" (number->string n)))
    ))

(define (otype->string ty)
  (cases optional-type ty
    (no-type () "?")
    (a-type (t) (type->string t))))

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
;; Page: 70
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

;; Interpreter for LETREC

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
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
      (proc-exp (var ty body)
                (proc-val (procedure var body env)))
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg)))
      (letrec-exp (ty1 p-name b-var ty2 p-body letrec-body)
          (value-of letrec-body
            (extend-env-rec p-name b-var p-body env)))
      )))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))

;; ---------- Type inferencer -----------------------------------
;;;;;;;;;;;;;;;; Unit substitution ;;;;;;;;;;;;;;;;

;; apply-one-subst: type * tvar * type -> type
;; (apply-one-subst ty0 var ty1) returns the type obtained by
;; substituting ty1 for every occurrence of tvar in ty0.  This is
;; sometimes written ty0[tvar=ty1]

;; apply-one-subst : Type * Tvar * Type -> Type
;; Page: 260
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
                 (proc-type
                  (apply-one-subst arg-type tvar ty1)
                  (apply-one-subst result-type tvar ty1)))
      (tvar-type (sn)
                 (if (equal? ty0 tvar) ty1 ty0)))))

;;;;;;;;;;;;;;;; Substitutions ;;;;;;;;;;;;;;;;

;; a substitution is a map from unknown types to types.
;; we'll represent this as an association list.

(define pair-of
  (lambda (pred1 pred2)
    (lambda (val)
      (and (pair? val) (pred1 (car val)) (pred2 (cdr val))))))

(define substitution? 
  (list-of (pair-of tvar-type? type?)))

;; basic observer: apply-subst-to-type
;; this is sometimes written ty1.subst 

;; apply-subst-to-type : Type * Subst -> Type
;; Page: 261
(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (t1 t2)
                 (proc-type
                  (apply-subst-to-type t1 subst)
                  (apply-subst-to-type t2 subst)))
      (tvar-type (sn)
                 (let ((tmp (assoc ty subst)))
                   (if tmp
                       (cdr tmp)
                       ty))))))

;; empty-subst : () -> Subst
;; produces a representation of the empty substitution.

;; extend-subst : Subst * Tvar * Type -> Subst

;; (extend-subst s tv t) produces a substitution with the property
;; that for all t0,

;;   (apply-subst t0 (extend-subst s tv t))
;;   = (apply-one-subst (apply-subst t0 s) tv t)

;; i.e.,  t0.(s[tv=t]) = (t0.s)[tv=t]

;; this means that for any type variable tv0 in the domain of s,

;;   (apply-subst tv0 (extend-subst s tv t))
;;   = (apply-one-subst (apply-subst tv0 s) tv t)

;; so we extend the substitution with a new element, and apply [t/v] to every
;; element already in the substitution. 


;; empty-subst : () -> Subst
;; Page 262
(define empty-subst (lambda () '()))

;; extend-subst : Subst * Tvar * Type -> Subst
;; usage: tvar not already bound in subst.
;; Page: 262
(define extend-subst
  (lambda (subst tvar ty)
    (cons
     (cons tvar ty)
     (map 
      (lambda (p)
        (let ((oldlhs (car p))
              (oldrhs (cdr p)))
          (cons
           oldlhs
           (apply-one-subst oldrhs tvar ty))))
      subst))))

; UNIFICATION

;; we'll maintain the invariant that no variable bound in the
;; substitution occurs in any of the right-hand sides of the
;; substitution. 


;;;;;;;;;;;;;;;; the unifier ;;;;;;;;;;;;;;;;

;; unifier : Type * Type * Subst * Exp -> Subst OR Fails
;; Page: 264
(define unifier
  (lambda (ty1 ty2 subst exp)
    (let ((ty1 (apply-subst-to-type ty1 subst))
          (ty2 (apply-subst-to-type ty2 subst)))
      (cond
        ((equal? ty1 ty2) subst)            
        ((tvar-type? ty1)
         (if (no-occurrence? ty1 ty2)
             (extend-subst subst ty1 ty2)
             (report-no-occurrence-violation ty1 ty2 exp)))
        ((tvar-type? ty2)
         (if (no-occurrence? ty2 ty1)
             (extend-subst subst ty2 ty1)
             (report-no-occurrence-violation ty2 ty1 exp)))
        ((and (proc-type? ty1) (proc-type? ty2))
         (let ((subst (unifier
                       (proc-type->arg-type ty1)
                       (proc-type->arg-type ty2)
                       subst exp)))
           (let ((subst (unifier
                         (proc-type->result-type ty1)
                         (proc-type->result-type ty2)
                         subst exp)))
             subst)))
        (else (report-unification-failure ty1 ty2 exp))))))

(define report-unification-failure
  (lambda (ty1 ty2 exp) 
    (eopl:error 'unification-failure
                "Type mismatch: ~s doesn't match ~s in ~s~%"
                (type->string ty1)
                (type->string ty2)
                exp)))

(define report-no-occurrence-violation
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-no-occurence!
                "Can't unify: type variable ~s occurs in type ~s in expression ~s~%" 
                (type->string ty1)
                (type->string ty2)
                exp)))

;; no-occurrence? : Tvar * Type -> Bool
;; usage: Is there an occurrence of tvar in ty?
;; Page: 265
(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      (int-type () #t)
      (bool-type () #t)
      (proc-type (arg-type result-type)
                 (and
                  (no-occurrence? tvar arg-type)
                  (no-occurrence? tvar result-type)))
      (tvar-type (serial-number) (not (equal? tvar ty))))))

;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

;; we'll be thinking of the type of an expression as pair consisting
;; of a type (possibly with some type variables in it) and a
;; substitution that tells us how to interpret those type variables.

;; Answer = Type * Subst
;; type-of: Exp * Tenv * Subst  -> Answer

(define-datatype answer answer?
  (an-answer                       
   (type type?)
   (subst substitution?)))

;; type-of-program : Program -> Type
;; Page: 267
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (cases answer (type-of exp1 (init-tenv) (empty-subst))
                   (an-answer (ty subst)
                              (apply-subst-to-type ty subst)))))))


;; type-of : Exp * Tenv * Subst -> Answer
;; Page: 267--270
(define type-of
  (lambda (exp tenv subst)
    (cases expression exp
      
      (const-exp (num) (an-answer (int-type) subst))
      
      (zero?-exp (exp1)
                 (cases answer (type-of exp1 tenv subst)
                   (an-answer (type1 subst1)
                              (let ((subst2 (unifier type1 (int-type) subst1 exp)))
                                (an-answer (bool-type) subst2)))))
      
      (diff-exp (exp1 exp2)
                (cases answer (type-of exp1 tenv subst)
                  (an-answer (type1 subst1)
                             (let ((subst1 (unifier type1 (int-type) subst1 exp1)))
                               (cases answer (type-of exp2 tenv subst1)
                                 (an-answer (type2 subst2)
                                            (let ((subst2
                                                   (unifier type2 (int-type) subst2 exp2)))
                                              (an-answer (int-type) subst2))))))))
      
      (if-exp (exp1 exp2 exp3)
              (cases answer (type-of exp1 tenv subst)
                (an-answer (ty1 subst)
                           (let ((subst (unifier ty1 (bool-type) subst
                                                 exp1)))
                             (cases answer (type-of exp2 tenv subst)
                               (an-answer (ty2 subst)
                                          (cases answer (type-of exp3 tenv subst)
                                            (an-answer (ty3 subst)
                                                       (let ((subst (unifier ty2 ty3 subst exp)))
                                                         (an-answer ty2 subst))))))))))
      
      (var-exp (var) (an-answer (apply-tenv tenv var) subst))
      
      (let-exp (var exp1 body)
               (cases answer (type-of exp1 tenv subst)
                 (an-answer (rhs-type subst)
                            (type-of body
                                     (extend-tenv var rhs-type tenv)
                                     subst))))
      
      (proc-exp (var otype body)
                (let ((arg-type (otype->type otype)))
                  (cases answer (type-of body
                                         (extend-tenv var arg-type tenv)
                                         subst)
                    (an-answer (result-type subst)
                               (an-answer
                                (proc-type arg-type result-type)
                                subst)))))
      
      (call-exp (rator rand)
                (let ((result-type (fresh-tvar-type)))
                  (cases answer (type-of rator tenv subst)
                    (an-answer (rator-type subst)
                               (cases answer (type-of rand tenv subst)
                                 (an-answer (rand-type subst)
                                            (let ((subst
                                                   (unifier rator-type
                                                            (proc-type rand-type result-type)
                                                            subst
                                                            exp)))
                                              (an-answer result-type subst))))))))
      
      (letrec-exp (proc-result-otype proc-name 
                                     bvar proc-arg-otype 
                                     proc-body
                                     letrec-body)
                  (let ((proc-result-type
                         (otype->type proc-result-otype)) 
                        (proc-arg-type
                         (otype->type proc-arg-otype)))
                    (let ((tenv-for-letrec-body
                           (extend-tenv 
                            proc-name
                            (proc-type proc-arg-type proc-result-type)
                            tenv)))
                      (cases answer (type-of proc-body
                                             (extend-tenv
                                              bvar proc-arg-type tenv-for-letrec-body)
                                             subst)
                        (an-answer (proc-body-type subst)
                                   (let ((subst 
                                          (unifier proc-body-type proc-result-type subst
                                                   proc-body))) 
                                     (type-of letrec-body
                                              tenv-for-letrec-body
                                              subst)))))))
      
      )))

;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
   (sym symbol?)
   (type type?)
   (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
                         (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (sym1 val1 old-env)
                            (if (eqv? sym sym1) 
                                val1
                                (apply-tenv old-env sym))))))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type) 
                 (extend-tenv 'v (int-type)
                              (extend-tenv 'i (int-type)
                                           (empty-tenv))))))

;; fresh-tvar-type : () -> Type
;; Page: 265  
(define fresh-tvar-type
  (let ((sn 0))
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

;; otype->type : OptionalType -> Type
;; Page: 265
(define otype->type
  (lambda (otype)
    (cases optional-type otype
      (no-type () (fresh-tvar-type))
      (a-type (ty) ty))))


;;; ======== ANYTHING BELOW THIS LINE IS SUPPORT CODE =========
(require racket/exn)

;; apply f to all types in exp
(define (map-exp f exp)
  (cases expression exp     
      (const-exp (num) (const-exp num))   
      (zero?-exp (exp1) (zero?-exp (map-exp f exp1)))   
      (diff-exp (exp1 exp2) (diff-exp (map-exp f exp1) (map-exp f exp2)))
      (if-exp (exp1 exp2 exp3)
              (if-exp (map-exp f exp1) (map-exp f exp2) (map-exp f exp3)))
      (var-exp (var) (var-exp var))
      (let-exp (var exp1 body)
               (let-exp var (map-exp f exp1) (map-exp f body)))
      (proc-exp (var otype body)
                (proc-exp var (f otype) (map-exp f body)))
      (call-exp (rator rand)
                (call-exp (map-exp f rator) (map-exp f rand)))
      (letrec-exp (proc-result-otype proc-name 
                                     bvar proc-arg-otype 
                                     proc-body
                                     letrec-body)
                  (letrec-exp (f proc-result-otype)
                              proc-name
                              bvar (f proc-arg-otype)
                              (map-exp f proc-body)
                              (map-exp f letrec-body)))
      ))


; replace no-types with typevars
(define notype->typevar
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program (map-exp otype->otype exp1))))))

(define otype->otype
  (lambda (otype)
    (cases optional-type otype
      (no-type () (a-type (fresh-tvar-type)))
      (a-type (ty) (a-type ty)))))

(define (program->program-with-inferred-types p)
  (cases program (notype->typevar p) 
  (a-program (exp1)
             (cases answer (type-of exp1 (init-tenv) (empty-subst))
                     (an-answer (ty subst)
                                (a-program 
                                 (map-exp (lambda (t) (a-type (apply-subst-to-type (rem-a t) subst))) exp1)))))))
                                
  
; top: Pgm -> String
(define print-results
  (lambda (p)
    (let ((pgm (notype->typevar p)))
      (display (string-append "PROGRAM WITH TYPEVARS:\n" 
                              (program->string pgm)
                              "\n"))
      (cases program pgm
        (a-program (exp1)
                   (with-handlers ([(λ (e) #t) (λ (e) (display (exn->string e)))]) 
                   (cases answer (type-of exp1 (init-tenv) (empty-subst))
                     (an-answer (ty subst)
                                (display (string-append
                                 "PROGRAM WITH INFERRED TYPES:\n"
                                 (program->string (a-program 
                                                   (map-exp (lambda (t) (a-type (apply-subst-to-type (rem-a t) subst))) exp1)))
                                 "\nTYPE:\n"
                                 (type->string (apply-subst-to-type ty subst))))
                                ))))))))

(define (rem-a  t)
  (cases optional-type t
      (no-type () '())
      (a-type (ty) ty)))

(define (test p)
  (begin
    (display "PROGRAM:\n")
    (display (program->string p))
    (display "\n")
    (print-results p)
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
    (proc-exp (var ty body)
              (let ((val (exp->doc body)))
                (group (h-append
                 (kw "proc")
                 (align (h-append
                 lparen
                 (text (symbol->string var)) colon (text (otype->string ty))
                 rparen
                 line
                 val))
                 ))))
    (call-exp (rator rand)
              (let ((val1 (exp->doc rator))
                    (val2 (exp->doc rand)))
                (group (h-append lparen space (align (h-append val1 line (align (h-append val2 space rparen))))))))
    (letrec-exp (rty p-name b-var pty p-body letrec-body)
                (let ((p-val (exp->doc p-body))
                      (letrec-val (exp->doc letrec-body)))
                  (h-append
                   (kw "letrec")
                   space
                   (text (otype->string rty)) space (text (symbol->string p-name))
                   lparen
                   (text (symbol->string b-var)) colon (text (otype->string pty))
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
                                       (let ((val (exp->doc body))) 
                                         (group (h-append
                                                 (kw "proc")
                                                 (align (h-append
                                                         lparen
                                                         (text (symbol->string var)) 
                                                         rparen
                                                         line
                                                         val))
                                                 )))))))                                                      
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

(define KEYWORDS (list "in" "let" "zero?" "if" "then" "else" "proc" "letrec" "int" "bool"))

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

(define ptype
  (<any>
   (parser-compose (token "int")
                   (return (int-type)))
   (parser-compose (token "bool")
                   (return (bool-type)))
   (parser-compose
    (token "(")
    (ty1 <- ptype)
    (token "->")
    (ty2 <- ptype)
    (token ")")
    (return (proc-type  ty1 ty2)))   
   ))

(define potype
  (<any>
   (parser-compose (token "?")
                   (return (no-type)))
   (parser-compose (token "int")
                   (return (a-type (int-type))))
   (parser-compose (token "bool")
                   (return (a-type (bool-type))))
   (parser-compose
    (token "(")
    (ty1 <- ptype)
    (token "->")
    (ty2 <- ptype)
    (token ")")
    (return (a-type (proc-type  ty1 ty2))))   
   ))
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
                   (rty <- potype)
                   (p-name <- psymbol)
                   (token "(")
                   (v-name <- psymbol) (token ":") (pty <- potype)
                   (token ")")
                   (token "=")
                   (p-body <- exp-parser)
                   (token "in")
                   (body <- exp-parser)
                   (return (letrec-exp rty p-name v-name pty p-body body)))   
   (parser-compose (token "let")
                   (v <- psymbol)
                   (token "=")
                   (e1 <- exp-parser)
                   (token "in")
                   (e2 <- exp-parser)
                   (return (let-exp v e1 e2)))
   (parser-compose (token "proc")
                   (token "(")
                   (v <- psymbol) (token ":") (ty <- potype)
                   (token ")")
                   (body <- exp-parser)
                   (return (proc-exp v ty body)))
   (parser-compose (v <- psymbol)
                   (return (var-exp v)))
   )
  )

(define (string->program s)
  (a-program (parse-result exp-parser s)))

;;; Example programs
(require "INFERRED-examples.rkt")
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
                    (tr (th "Description") (th "Program") (th "Inferred types") (th "Type of program") (th "Edit"))
                    ,@(map (λ (p) `(tr
                                    (td ,(first p))
                                    (td (pre ,(program->html (string->program (second p)))))
                                    (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (program->html (program->program-with-inferred-types (string->program (second p)))) ))
                                    (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (type->string (type-of-program (string->program (second p)))) ))
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
              (input ((type "submit"))))
                  (tr (th "Source Program") )
                  (tr (td (pre ,(program->html p))))
                  (tr (th "Inferred types") )
                  (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (program->html (program->program-with-inferred-types p)) )))
                  (tr (th "Type"))
                  (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (type->string (type-of-program p)) )))
                                  ) 
                  ))))
   ))))

(define (web) (serve/dispatch dispatch))
