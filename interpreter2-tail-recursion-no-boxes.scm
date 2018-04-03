;Jason Harnack, Vicki Kelly, Tom Pescatore
;NOTE: UTILIZING ALL COMMENTS AND CODE FROM GIVEN INTERPRETER SOLUTION IN CLASS

;(load "simpleParser.scm")
(load "functionParser.scm")

; An interpreter for the simple language using tail recursion for the M_state functions and does not handle side effects.

; The functions that start interpret-...  all return the current environment.  These are the M_state functions.
; The functions that start eval-...  all return a value.  These are the M_value and M_boolean functions.

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  Sets default continuations for return, break, continue, throw, and "next statement"
(define interpret
  (lambda (file)
    (scheme->language
     (interpret-statement-list '((funccall main)) (outer-interpret file) (lambda (v) v) (lambda (v) v)
                               (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                               (lambda (v env) (myerror "Uncaught exception thrown")) (lambda (env) env)))))

(define outer-interpret
  (lambda (file)
    (scheme->language
     (interpret-outer-state-list (parser file) (newenvironment) (lambda (v) v)
                                 (lambda (v env) (myerror "Inproper Use of Throw"))
                                 (lambda (env) env))))) 

(define interpret-outer-state-list
  (lambda (statement-list environment return throw next)
    (if (null? statement-list)
        (return environment)
        (interpret-outer-state (first-statement statement-list) environment (lambda (v) v) throw (lambda (env) (interpret-outer-state-list (next-statement statement-list) env return throw next))))))

(define first-statement car)
(define next-statement cdr)
 
 
; interprets a list of statements.  The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return function-return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (first-statement statement-list) environment return function-return break continue throw (lambda (env) (interpret-statement-list (next-statement statement-list) env return function-return break continue throw next))))))

;Handles all outer state/global assignments and functions
(define interpret-outer-state
  (lambda (statement environment return throw next)
    (cond
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment next return throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment next))
      ((eq? 'function (statement-type statement)) (interpret-function-def statement environment next))
      (else (myerror "Unexpected Statement outside of functions")))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define interpret-statement
  (lambda (statement environment return function-return break continue throw next)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment function-return throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment next return throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment next throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return function-return break continue throw next))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return function-return throw next))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return function-return break continue throw next))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return function-return break continue throw next))
      ((eq? 'function (statement-type statement)) (interpret-func-def statement environment return next))
      ((eq? 'funccall (statement-type statement)) (interpret-func-call statement environment return break continue throw next))
      (else (myerror "Unknown statement:" (statement-type statement))))))


;Creating within the environment a binding of (function-name function-closure)
(define interpret-function-def
  (lambda (statement environment return)
    (return (create-binding (get-function-name statement) (create-closure statement environment) environment))))

(define get-function-name cadr)

(define create-binding
  (lambda (key val environment)
    (cond
      ((exists? key (list (topframe environment))) (cons (update-binding-layer key value (topframe environment)) (popframe environment)))
      (else (insert key val environment)))))

(define create-closure
  (lambda (statement environment)
    (list (parameters statement) (code statement) (function-environment))))

(define parameters caddr)
(define code cadddr)

(define function-environment
  (lambda ()
    ((lambda (f)
       (f f))
     (lambda (a)
       (lambda (formal-params actual-params environment)
         (if (null? formal-params) environment
             ((a a) (next-parameters formal-params) (next-parameters actual-params) (insert (first-parameter formal-params) (first-parameter actual-params) environment))))))))

(define first-parameter car)
(define next-parameters cdr)
       

(define update-binding-layer
  (lambda (key val environment) key))

;Mstate for calling functions
;Ya idk
(define interpret-func-call
  (lambda (statement environment return break continue throw next)
    (begin
      (get-closure (function-name statement) environment (lambda (v) (interpret-function-with-return
       (get-function-body v) ((get-func-environment v) (get-function-params v) (get-actual-params statement) environment) return break continue throw next))))));statement, environment, return, break, continue, throw,next

(define function-name cadr)
(define get-func-environment caddr)
(define get-function-params car)
(define get-function-body cadr)
(define get-actual-params
  (lambda (statement)
    (if (null? (cddr statement))
        '()
        (caddr statement))))

(define get-formal-params cadddr)

(define get-closure
  (lambda (func-name environment return)
    (cond
      ((exists? func-name (list (topframe environment))) (return (get-binding func-name environment)))
      (else (myerror "Not within scope")))))

(define get-binding
  (lambda (func-name environment)
    (lookup-variable func-name environment)))
          
(define get-function-environment
  (lambda (statement environment)
    ((caddr (get-closure (function-name statement) environment)) environment)))           
    
;Creates immediate return continuation for returning in order to exit function
(define interpret-function-with-return
  (lambda (statement environment return break continue throw next)
    (call/cc
     (lambda (funcReturn)
       (interpret-statement-list statement environment return funcReturn break continue throw next)))))

;Mvalue for function calling
(define eval-func-call
  (lambda (statement environment return throw)
    (letrec ((function-closure (get-closure (cadr statement) environment)))
    (cond
      ((eq? function-closure 'error) (myError "Undefined Function"))
      (else (return (get-binding 'return (interpret-function-with-return
       (get-function-body (get-closure (function-name statement) environment))
       (bind-parameters (get-actual-params statement)
                        (get-formal-params statement)
                        (get-function-environment statement environment)
                        environment return throw next)))))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw)
    (return (eval-expression (get-expr statement) environment return throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment next return throw)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment return throw) environment))
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement environment next throw)
    (next (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return function-return break continue throw next)
    (cond
      ((eval-expression (get-condition statement) environment) (interpret-statement (get-then statement) environment return function-return break continue throw next))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return function-return break continue throw next))
      (else (next environment)))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return function-return throw next)
    (letrec ((loop (lambda (condition body environment)
                     (if (eval-expression condition environment)
                         (interpret-statement body environment return function-return (lambda (env) (next env)) (lambda (env) (loop condition body env)) throw (lambda (env) (loop condition body env)))
                         (next environment)))))
      (loop (get-condition statement) (get-body statement) environment))))

; Interprets a block.  The break, continue, throw and "next statement" continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return function-return break continue throw next)
    (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         function-return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))
                                         (lambda (env) (next (pop-frame env))))))

; We use a continuation to throw the proper value.  Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw next finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (interpret-block finally-block env return break continue throw (lambda (env2) (throw ex env2))))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
                  (interpret-statement-list 
                       (get-body catch-statement) 
                       (insert (catch-var catch-statement) ex (push-frame env))
                       return 
                       (lambda (env2) (break (pop-frame env2))) 
                       (lambda (env2) (continue (pop-frame env2))) 
                       (lambda (v env2) (throw v (pop-frame env2))) 
                       (lambda (env2) (interpret-block finally-block (pop-frame env2) return break continue throw next))))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return function-return break continue throw next)
    (let* ((finally-block (make-finally-block (get-finally statement)))
           (try-block (make-try-block (get-try statement)))
           (new-return (lambda (v) (interpret-block finally-block environment return break continue throw (lambda (env2) (return v)))))
           (new-break (lambda (env) (interpret-block finally-block env return break continue throw (lambda (env2) (break env2)))))
           (new-continue (lambda (env) (interpret-block finally-block env return break continue throw (lambda (env2) (continue env2)))))
           (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw next finally-block)))
      (interpret-block try-block environment new-return new-break new-continue new-throw (lambda (env) (interpret-block finally-block env return break continue throw next))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment return throw)
    (cond
      ((number? expr) (return expr))
      ((eq? expr 'true) (return #t))
      ((eq? expr 'false) (return #f))
      ((eq? expr 'funccall) (eval-func-call expr environment return throw))
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment return throw)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment return throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment return throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment return throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment return throw) environment return throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment return throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment return throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment return throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment return throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (unbox (car l)))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))  
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language (box val)) (store frame)))))
;(list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (set-box! (car vallist) val)) ;(cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))


; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

