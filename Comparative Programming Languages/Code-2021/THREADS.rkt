#lang racket
(require eopl)
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
  (a-program (exp1 expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (unop-exp (oper unop?) (exp1 expression?)) ; Unary operators
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
  (letrec-exp ; this letrec supports multiple mutually recursive definitions
   (p-name (list-of symbol?))
   (b-var (list-of symbol?))
   (p-body (list-of expression?))
   (letrec-body expression?))
  (const-list-exp (nums (list-of number?)))
  (begin-exp (exp1 expression?) (exps (list-of expression?)))
  (assign-exp
   (var symbol?)
   (exp expression?))
  (spawn-exp (exp1 expression?))
  (yield-exp)
  (mutex-exp)
  (wait-exp (exp1 expression?))
  (signal-exp (exp2 expression?))  
  )

(define-datatype unop unop?
  (car-unop)
  (cdr-unop)
  (null?-unop)
  (zero?-unop)
  (print-unop))

(define (oper->string oper)
  (cases unop oper
    (null?-unop () "null?")
   (car-unop () "car")
   (cdr-unop () "cdr")
   (zero?-unop () "zero?")
    (print-unop () "print")))


;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

;;; world's dumbest model of the store:  the store is a list and a
;;; reference is number which denotes a position in the list.

;; the-store: a Scheme variable containing the current state of the
;; store.  Initially set to a dummy variable.
(define the-store 'uninitialized)
(define the-output 'uninitialized) ; collecting output from print statements

;; empty-store : () -> Sto
;; Page: 111
(define empty-store
  (lambda () '()))
(define empty-output
  (lambda() '()))

;; initialize-store! : () -> Sto
;; usage: (initialize-store!) sets the-store to the empty-store
;; Page 111
(define initialize-store!
  (lambda ()
    (set! the-output (empty-output))
    (set! the-store (empty-store))))

(define (myprint! n)
  (set! the-output (cons n the-output)))

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
(define get-output
  (lambda ()
    (reverse the-output)))

;; Semantics
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (list-val
   (lst (list-of expval?)))
  (mutex-val
   (mutex mutex?))
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

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (else (expval-extractor-error 'list v)))))

(define expval->mutex
  (lambda (v)
    (cases expval v
      (mutex-val (l) l)
      (else (expval-extractor-error 'mutex v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; mutexes ;;;;;;;;;;;;;;;;

(define-datatype mutex mutex?
  (a-mutex
   (ref-to-closed?    reference?)    ; ref to bool
   (ref-to-wait-queue reference?)))  ; ref to (listof thread)

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

(define-datatype continuation continuation?
  
  (end-main-thread-cont)           
  (end-subthread-cont) 
  (diff1-cont                       
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont                         
   (val1 expval?)
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
  (set-rhs-cont
   (loc reference?)
   (cont continuation?))
  (spawn-cont 
   (saved-cont continuation?))
  (wait-cont 
   (saved-cont continuation?))
  (signal-cont 
   (saved-cont continuation?))
  (unop-arg-cont
   (unop1 unop?)
   (cont continuation?))
  )

;; Environments
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval reference?)                 
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
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
      (extend-env-rec* (p-names b-vars p-bodies saved-env)
                       (let ((n (location search-sym p-names)))
                         ;; n : (maybe int)
                         (if n
                             (newref
                              (proc-val
                               (procedure 
                                (list-ref b-vars n)
                                (list-ref p-bodies n)
                                env)))
                             (apply-env saved-env search-sym))))
      )))

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


;; init-env : () -> Env

(define init-env 
  (lambda ()
    (extend-env 
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
       (empty-env))))))

;; queues

;; We maintain the queue by adding to the end and dequeuing from the
;; front. 

;; exercise: enqueue is expensive, since it uses append.  Do
;; something better than this.

(define empty-queue
  (lambda ()
    '()))

(define empty? null?)

(define enqueue
  (lambda (q val)
    (append q (list val))))

(define dequeue
  (lambda (q f)
    (f (car q) (cdr q))))

;;;;;;;;;;;;;;;; the scheduler ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;

;; components of the scheduler state:

(define the-ready-queue   'uninitialized)         
(define the-final-answer  'uninitialized)

(define the-max-time-slice    'uninitialized)
(define the-time-remaining    'uninitialized)

;; initialize-scheduler! : Int -> Unspecified
(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice) 
    ))

;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

;; place-on-ready-queue! : Thread -> Unspecified
;; Page: 184  
(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
          (enqueue the-ready-queue th))
    (display (string-append "RQ: " (number->string (length the-ready-queue)) "\n"))
    ))

;; run-next-thread : () -> FinalAnswer
;; Page: 184    
(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
                 (lambda (first-ready-thread other-ready-threads)
                   (set! the-ready-queue other-ready-threads)            
                   (set! the-time-remaining the-max-time-slice) 
                   (first-ready-thread)
                   )))))

;; set-final-answer! : ExpVal -> Unspecified
;; Page: 184    
(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))

;; time-expired? : () -> Bool
;; Page: 184    
(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

;; decrement-timer! : () -> Unspecified
;; Page: 184    
(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))


;; new-mutex () -> Mutex
;; Page: 188
(define new-mutex
  (lambda ()
    (a-mutex
     (newref #f)                     
     (newref '()))))                 

; wait queue, initially empty

;; wait-for-mutex : Mutex * Thread -> FinalAnswer
;; waits for mutex to be open, then closes it.
;; Page: 190
(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (cond
                 ((deref ref-to-closed?)                  
                  (setref! ref-to-wait-queue
                           (enqueue (deref ref-to-wait-queue) th))
                  (run-next-thread))
                 (else
                  (setref! ref-to-closed? #t)
                  (th)))))))

;; signal-mutex : Mutex * Thread -> FinalAnswer
;; Page 190
(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (let ((closed? (deref ref-to-closed?))
                     (wait-queue (deref ref-to-wait-queue)))
                 (cond (closed? 
                     (if (empty? wait-queue)
                         (setref! ref-to-closed? #f)
                         (dequeue wait-queue
                                  (lambda (first-waiting-th other-waiting-ths)
                                    (place-on-ready-queue!
                                     first-waiting-th)
                                    (setref!
                                     ref-to-wait-queue
                                     other-waiting-ths)))))
                     )
                 (th))))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;


;; value-of-program : Program * Int -> ExpVal
;; Page: 185    
(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
                 (value-of/k
                  exp1
                  (init-env)
                  (end-main-thread-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page 182
(define value-of/k                    
  (lambda (exp env cont)
    (display (string-append "VOK: exp = " (exp->string exp) " , cont = " (cont->string cont) " \n" ))
    (cases expression exp
      
      (const-exp (num) (apply-cont cont (num-val num)))
      
      (const-list-exp (nums)
                      (apply-cont cont
                                  (list-val (map num-val nums))))
      
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env
                            (diff1-cont exp2 env cont)))
      
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
      
      (begin-exp (exp exps)           ; desugar to let expression
                 (if (null? exps)
                     (value-of/k exp env cont)
                     (value-of/k
                      (let-exp 'void  ; void should not be used as variable
                               exp
                               (begin-exp (car exps) (cdr exps)))
                      env
                      cont)))
      
      (letrec-exp (p-names b-vars p-bodies letrec-body)
                  (value-of/k
                   letrec-body
                   (extend-env-rec* p-names b-vars p-bodies env)
                   cont))
      
      (assign-exp (id exp)
               (value-of/k exp env
                           (set-rhs-cont (apply-env env id) cont)))
      
      (spawn-exp (exp)
                 (value-of/k exp env
                             (spawn-cont cont)))
      
      (yield-exp ()
                 (place-on-ready-queue!
                  (lambda () (apply-cont cont (num-val -1))))
                 (run-next-thread))
      
      (mutex-exp ()
                 (apply-cont cont (mutex-val (new-mutex))))  
      
      (wait-exp (exp)
                (value-of/k exp env
                            (wait-cont cont)))
      
      (signal-exp (exp)
                  (value-of/k exp env
                              (signal-cont cont)))
      
      (unop-exp (unop1 exp)
                (value-of/k exp env
                            (unop-arg-cont unop1 cont)))
      
      )))

;; apply-cont : Cont * Exp -> FinalAnswer
;; Page: 182 and 186
(define apply-cont                    
  (lambda (cont val)
    (display (string-append "AC: cont = " (cont->string cont) " , val = " (expval->string val) " \n" ))
    (if (time-expired?)
        (begin
          (place-on-ready-queue!
           (lambda () (apply-cont cont val)))
          (run-next-thread))
        (begin
          
          (decrement-timer!)
          
          (cases continuation cont
            
            (end-main-thread-cont ()
                                  (set-final-answer! val)
                                  (run-next-thread))
            
            (end-subthread-cont ()
                                (run-next-thread))
            
            (diff1-cont (exp2 saved-env saved-cont)
                        (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
            (diff2-cont (val1 saved-cont)
                        (let ((n1 (expval->num val1))
                              (n2 (expval->num val)))
                          (apply-cont saved-cont
                                      (num-val (- n1 n2)))))
            (if-test-cont (exp2 exp3 env cont)
                          (if (expval->bool val)
                              (value-of/k exp2 env cont)
                              (value-of/k exp3 env cont)))
            (let-exp-cont (var body saved-env saved-cont)
                          (value-of/k body
                                      (extend-env var (newref val) saved-env) saved-cont))
            (rator-cont (rand saved-env saved-cont)
                        (value-of/k rand saved-env
                                    (rand-cont val saved-cont)))
            (rand-cont (val1 saved-cont)
                       (let ((proc (expval->proc val1)))
                         (apply-procedure proc val saved-cont)))
            (set-rhs-cont (loc cont)
                          (begin
                            (setref! loc val)
                            (apply-cont cont (num-val 26))))
            
            (spawn-cont (saved-cont)
                        (let ((proc1 (expval->proc val)))
                          (place-on-ready-queue!
                           (lambda ()
                             (apply-procedure proc1
                                              (num-val 28)
                                              (end-subthread-cont))))
                          (apply-cont saved-cont (num-val 73))))
            
            (wait-cont (saved-cont)
                       (wait-for-mutex
                        (expval->mutex val)
                        (lambda () (apply-cont saved-cont (num-val 52)))))
            
            (signal-cont (saved-cont)
                         (signal-mutex
                          (expval->mutex val)
                          (lambda () (apply-cont saved-cont (num-val 53)))))
            
            (unop-arg-cont (unop1 cont)
                           (apply-unop unop1 val cont))
            
            )))))

(define apply-procedure
  (lambda (proc1 arg cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body
                             (extend-env var (newref arg) saved-env)
                             cont)))))

(define apply-unop
  (lambda (unop1 arg cont)
    (cases unop unop1
      
      (zero?-unop ()
                  (apply-cont cont
                              (bool-val
                               (zero? (expval->num arg)))))
      
      (car-unop ()
                (let ((lst (expval->list arg)))
                  (apply-cont cont (car lst))))
      (cdr-unop ()
                (let ((lst (expval->list arg)))
                  (apply-cont cont (list-val (cdr lst)))))
      
      (null?-unop ()
                  (apply-cont cont 
                              (bool-val (null? (expval->list arg)))))
      
      (print-unop ()
                  (begin
                    ;(eopl:printf "~a~%" (expval->num arg))
                    (myprint! (expval->num arg)) ; appends to global the-output variable
                    (apply-cont cont (num-val 1))))
      
      )))



;;; ======== ANYTHING BELOW THIS LINE IS SUPPORT CODE =========
(require racket/exn)
(define (test timeslice p)
  (begin
    (display (program->string p))
    (display "evaluates to:\n")
    (display (with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->string (value-of-program timeslice p)) ))
    (display "\n----------------\n")))

(define (test-all)
  (map (λ (p n) (begin (display (string-append "Program " (number->string n) "\n")) (test 10 p))) examples (range 0 (length examples)) ))

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
    (letrec-exp (p-names b-vars p-bodies letrec-body)
                (let ((p-vals (map exp->doc p-bodies))
                      (letrec-val (exp->doc letrec-body)))
                  (h-append
                   (kw "letrec") line
                   space space
                   (align (apply v-append
                                 (map (λ (p-name b-var p-val) (h-append
                                        (text (symbol->string p-name))
                                        lparen
                                        (text (symbol->string b-var))
                                        rparen
                                        space
                                        equals
                                        space
                                        (align p-val)
                                        
                                        )) p-names b-vars p-vals)))
                   line
                   (kw "in")
                   space
                   (align letrec-val)) ))
    (const-list-exp (nums)
                    (apply hs-append
                           (append
                           (cons lbracket
                                 (apply-infix comma (map (λ (n) (text (number->string n))) nums)))
                           (list rbracket))
                           ))
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
    (spawn-exp (exp1)
               (let ((val1 (exp->doc exp1 )))
                 (h-append (kw "spawn") lparen space (align val1) space rparen)))
    (wait-exp (exp1)
               (let ((val1 (exp->doc exp1 )))
                 (h-append (kw "wait") lparen space (align val1) space rparen)))
    (signal-exp (exp1)
               (let ((val1 (exp->doc exp1 )))
                 (h-append (kw "signal") lparen space (align val1) space rparen)))
    (yield-exp ()
               (text "yield()"))
    (mutex-exp ()
               (text "mutex()"))
    ))

(define (cont->doc k)
  (define (inner k hole)
    (cases continuation k
      (end-main-thread-cont () hole)
      (end-subthread-cont () hole)
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
      (spawn-cont (k2)
         (inner k2 (hs-append (kw "spawn") lparen (align hole) rparen)))
      (signal-cont (k2)
                   (inner k2 (hs-append (kw "signal") lparen (align hole) rparen)))
      (wait-cont (k2)
         (inner k2 (hs-append (kw "wait") lparen (align hole) rparen)))
      (set-rhs-cont (loc k2)
                    (inner k2 
                           (hs-append (kw "set") (text (string-append "@" (number->string loc))) (char #\=) (align hole) )))   
      )
    )
  (inner k (kw "[ ]")))
    
(define (program->string p)
  (pretty-format (program->doc p)))

(define (expval->string v)
  (pretty-format (expval->doc v)))

(define (exp->string v)
  (pretty-format (exp->doc v)))

(define (cont->string v)
  (pretty-format (cont->doc v)))
(define (env->html env)
  (define (iter env)
    (cases environment env
      (empty-env () `())
      (extend-env (bvar bval saved-env)
                  (cons `(tr (td ,(symbol->string bvar)) (td ,(string-append "@" (number->string bval))))
                        (iter saved-env)))            
      (extend-env-rec* (p-names b-vars p-bodies saved-env)
                    (append
                     (map (λ (p-name b-var p-body) `(tr (td ,(string-append (symbol->string p-name) "(" (symbol->string b-var) ")")) (td ,(exp->html p-body)))) p-names b-vars p-bodies)
                        (iter saved-env))  
                    )))
  (cons 'table (cons `((class "w3-border")) (iter env)))
    )

(define (expval->doc v)
  (if (expval? v) ; because mutexes use the store to store a boolean and a queue and these are not expvals
      (cases expval v
        (bool-val (bool) (text (if bool "#t" "#f")))
        (num-val (n) (text (number->string n)))
        (proc-val (p) (cases proc p
                        (procedure (var body saved-env)
                                   (addenv saved-env ; only add env when converting to HTML
                                           (exp->doc (proc-exp var body))))))
        (list-val (nums) (apply hs-append (append (list lbracket) (add-between (map (λ (n) (expval->doc n)) nums) comma) (list rbracket))))
        (mutex-val (m) (cases mutex m
                         (a-mutex (r1 r2) (h-append lbracket (text "closed:") (text (number->string r1)) comma (text "queue:") (text (number->string r2)) rbracket))))
        )
      (if (list? v)
          (text (string-append "Queue with " (number->string (length v)) " elements."))
          (text (string-append "Closed?: " (if v "#t" "#f"))))))
  

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

(define KEYWORDS (list "in" "let" "zero?" "null?" "car" "cdr" "if" "then" "else" "proc" "letrec" "print" "spawn" "wait" "signal" "begin" "end" "yield" "set" "mutex"))

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
   (parser-compose (token "yield")(token "(") (token ")")
                   (return (yield-exp)))
   (parser-compose (token "mutex")(token "(") (token ")")
                   (return (mutex-exp)))
   (parser-compose (token "spawn")
                   (token "(")
                   (e <- exp-parser)
                   (token ")")
                   (return (spawn-exp e)))
   (parser-compose (token "wait")
                   (token "(")
                   (e <- exp-parser)
                   (token ")")
                   (return (wait-exp e)))
   (parser-compose (token "signal")
                   (token "(")
                   (e <- exp-parser)
                   (token ")")
                   (return (signal-exp e)))
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
   (parser-compose (token "print")
                   (token "(")
                   (e <- exp-parser)
                   (token ")")
                   (return (unop-exp (print-unop) e)))   
   (parser-compose (token "letrec")
                   (l <- (many1 (parser-compose
                                 (p-name <- psymbol)
                                 (token "(")
                                 (v-name <- psymbol)
                                 (token ")")
                                 (token "=")
                                 (p-body <- exp-parser)
                                 (return (list p-name v-name p-body)))))
                   (token "in")
                   (body <- exp-parser)
                   (return (letrec-exp (map first l) (map second l) (map third l) body)))   
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
   (parser-compose (token "begin")
                        (l <- (many1 (parser-compose (e <- exp-parser) (token ";") (return e))))
                        (token "end")
                        (return (begin-exp (first l) (cdr l) )))
   (try (parser-compose (token "set")
                        (v <- psymbol)
                        (token "=")
                        (e <- exp-parser)
                        (return (assign-exp v e))))
   (parser-compose (v <- psymbol)
                   (return (var-exp v)))
   )
  )

(define (string->program s)
  (a-program (parse-result exp-parser s)))

;;; Example programs
(require "THREADS-examples.rkt")
(define examples
  (map (compose string->program second) examples-with-notes))

(define (p n) (list-ref examples n))

;; Web server for playing with programs
(require web-server/servlet
         web-server/servlet-env)

(define-values (dispatch to-url)
    (dispatch-rules
     [("edit") edit-program]
     [("") list-programs]
     ;[else list-programs]
     ))

(define (list-programs req)
  (response/xexpr
   `(html
       (head
        (title "THREADS Programming Language")
        (link ((rel "stylesheet") (href "https://www.w3schools.com/w3css/4/w3.css"))))
       (body
        (div ((class "w3-container w3-responsive"))    
             (table ((class "w3-table-all"))
                    (tr (th ((width "20%")) "Description") (th ((width "50%")) "Program") (th ((width "20%")) "Result") (th ((width "10%")) "Edit"))
                    ,@(map (λ (p) `(tr
                                    (td ,(first p))
                                    (td (pre ,(program->html (string->program (second p)))))
                                    (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->html (value-of-program 10 (string->program (second p)))) ))
                                    (td (form ((action ,(to-url edit-program)))
                                              (input ((type "hidden") (name "prog") (value ,(second p))))
                                              (input ((type "hidden") (name "time") (value "10")))
                                              (input ((type "submit") (value "Edit"))))
                                     )
                                    )) examples-with-notes )
                    
                    ))))
   )
  )
(define (edit-program req)
  (let* ((p-string (extract-binding/single 'prog (request-bindings req)))
         (time-slice (string->number (extract-binding/single 'time (request-bindings req))))
         (p (string->program p-string))
         )
    (response/xexpr
     `(html
       (head
        (title "THREADS Programming Language")
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
                             "Time slice:" (input ((type "text") (name "time") (value ,(number->string time-slice))))
                             (input ((type "submit"))))
                            (tr (th "Source Program") )
                            (tr (td (pre ,(program->html p))))
                            (tr (th "Result") )
                            (tr (td ,(with-handlers ([(λ (e) #t) (λ (e) (exn->string e))]) (expval->html (value-of-program time-slice p)) )))
                            (tr (th "Output") )
                            (tr (td ,@(map (λ (n) (string-append (number->string n) " ")) (get-output))))
                            (tr (th "Store") )
                            (tr (td (ul ((class "w3-card-4")) ,@(map (λ (cell) `(li ,(string-append (number->string (first cell)) ":") ,(expval->html (second cell)))) (get-store-as-list)))))                                  )
                        ))))
       ))))


(define (web) (serve/dispatch dispatch))
