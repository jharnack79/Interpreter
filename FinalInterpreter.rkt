; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
; #lang racket
; (require "simpleParser.scm")
;(load "functionParser.scm")
(load "classParser.scm")

; An interpreter for the simple language using tail recursion for the M_state functions and does not handle side effects.

; The functions that start interpret-...  all return the current environment.  These are the M_state functions.
; The functions that start eval-...  all return a value.  These are the M_value and M_boolean functions.

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  Sets default continuations for return, break, continue, throw, and "next statement"
(define interpret
  (lambda (file)
    (scheme->language
     (interpret-statement-list '((funcall main)) (outer-interpret file) (lambda (v) v)
                               (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                               (lambda (v env) (myerror "Uncaught exception thrown")) (lambda (env) env)))))

;The main function for all global variables
(define outer-interpret
  (lambda (file className)
    (scheme->language
     (interpret-outer-state-list (parser file) (newenvironment) (lambda (v) v)
                                 (lambda (v env) (myerror "Inproper Use of Throw"))
                                 (lambda (env) env) className))))

;The main function for classes
(define class-interpret
  (lambda (file className)
    (scheme->language
     (interpret-statement-list '((funcall main)) (outer-interpret file className) (lambda (v) v)
                                 (lambda (v env) (myerror "Inprober use of Throw")) (lambda (env) (myerror "Continue used outside of loop"))
                                 (lambda (v env) (myerror "Uncaught exception thrown"))
                                 (lambda (env) env) className))))

;Interprets a list of statements to create global variables 
(define interpret-outer-state-list
  (lambda (statement-list environment return throw next className)
    (if (null? statement-list)
        (return environment)
        (interpret-outer-state (car statement-list) environment (lambda (v) v) throw (lambda (env) (interpret-outer-state-list (cdr statement-list) env return throw next className)) className))))

; interprets a list of statements.  The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw next className)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list) environment return break continue throw (lambda (env) (interpret-statement-list (cdr statement-list) env return break continue throw next className)) className))))


;Handles all outer state/global assignments and functions
(define interpret-outer-state
  (lambda (statement environment return throw next className)
    (cond
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return throw next className))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment return throw next))
      ((eq? 'function (statement-type statement)) (interpret-function-def statement environment next))
      ((eq? 'class (statement-type statement)) (interpret-class-def statement environment return next))
      (else (myerror "Unexpected Statement outside of functions")))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define interpret-statement
  (lambda (statement environment return break continue throw next className)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw next className))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return throw next className))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment return throw next))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw next className))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw next))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw next))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment return throw next))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw next))
      ((eq? 'function (statement-type statement)) (interpret-function-def statement environment next))
      ((eq? 'funcall (statement-type statement)) (interpret-func-call statement environment return throw next className))
      ((eq? 'dot (statement-type statement)) (interpret-dot statement environmnet return throw next className))
      (else (myerror "Unknown statement:" (statement-type statement))))))

(define interpret-dot
  (lambda (statement environment return throw next className)
    (next (create-binding 'this (get-binding (cadr statement)) environment))))
  
(define interpret-class-def
  (lambda (statement environment return next)
    (next (create-binding (cadr statement) (create-class-closure statement environment next (cadr statement)) environment))))

(define interpret-function-def
  (lambda (statement environment next)
    (next (create-binding (get-function-name statement) (create-closure statement environment) environment))))

(define get-function-name cadr)

;Mstate for calling functions & main
;(define interpret-func-call
;  (lambda (statement environment return break continue throw next)
;      (get-closure (function-name statement) environment
;                   (lambda (v) (interpret-function-with-return
;                                (get-function-body v)
;                                ((get-func-environment v) (get-function-params v) (get-actual-params statement) environment)
;                                return break continue throw next)))))

;Next being passed in could potentially not be correct
;What should the return be at this point?
;Do we need to pop the frame on the continue and break continuations?
; (null, staticMethods, instanceFields, instanceMethods)
(define interpret-func-call
  (lambda (statement environment return throw next className)
    (let ((instanceMethods (cadr (get-closure className environment))))
      (let ((instanceFields (caddr (get-closure className environment))))
        (interpret-statement-list (get-function-body (get-closure (function-name statement) instanceMethods))
                                  ((get-func-environment (get-closure (function-name statement) instanceMethods))
                                   (car (get-closure (function-name statement) instanceMethods)) (actual-parameters-list (get-actual-params statement) instanceFields return throw next) (push-frame environment) className)
                                  (lambda (env) (return env)) ;was (return (pop-frame env))
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  throw next className)))))

(define eval-func-call
  (lambda (statement environment return throw next className)
    (let ((instanceMethods (cadr (get-closure className environment))))
      (let ((instanceFields (caddr (get-closure className environment))))
        (return (interpret-statement-list (get-function-body (get-closure (function-name statement) instanceMethods))
                                          ((get-func-environment (get-closure (function-name statement) instanceMethods))
                                           (car (get-closure (function-name statement) instanceMethods)) (actual-parameters-list (get-actual-params statement) instanceFields return throw next) (push-frame environment))
                                          (lambda (env) (return env))
                                          (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                          throw next className))))))
;(define get-class-closure
 ; (lambda (className environment)
  ;  (cond
   ;   ((exists? className environment) (get-binding className environment))
    ;  (else (myerror "Class not within scope")))))


;removed return at one point, is it necessary?
(define get-closure
  (lambda (func-name environment)
    (cond
      ((exists? func-name environment) (get-binding func-name environment))
      (else (myerror "Not within scope")))))

;Create binding of function name to function enclosure
(define create-binding
  (lambda (functionName functionClosure environment)
    (insert functionName functionClosure environment)))

(define update-binding
  (lambda (name closure environment)
    (update name closure environment)))
      
;Creating the Closure ((formal parameters) (function body) (function that binds actual to formal paramters) (function that looks up funciton class)
(define create-closure
  (lambda (statement environment)
    (list (caddr statement) (cadddr statement) (function-environment))))

;Creating the closure for the classes
;Either will have parent class or not
;(class Name (extends B) body)
(define create-class-closure
  (lambda (statement environment next className) ;Do we pass in className or does it get pulled from statement?
    (cond
      ((null? (caddr statement))
       (create-class-body-closure (cadddr statement)
                                  (create-binding className new_class environment) next className))
      (else
       (create-class-body-closure (caddr statement)
                                  (create-binding className (cons (cadaddr statement) (new-class-without-parent new_class)) environment) next className)))))

(define new-class-without-parent cdr)
;New Class --> (parent, staticMethods, instanceFields instanceMethods)
(define new_class '( null ((() ())) ((() ())) ((() ())))  )

(define create-class-body-closure
  (lambda (statement environment next className)
    (cond
      ((null? statement) environment)
      ((eq? 'static-function (class-statement-type statement)) (create-static-method statement environment next className))
      ((eq? 'var (class-statement-type statement)) (create-instance-field statement environment className))
      ((eq? 'function (class-statement-type statement)) (create-instance-method statement environment next className))
      (else (class_body_def (get-class-body body) s className)))))

(define get-class-body cdr)
(define class-statement-type caar)
 
(define create-instance-method
  (lambda (statement environment next className)
    (let ((class-binding (get-binding className environment)))
    (list (parentClassName class-binding) (staticMethods class-binding) (instanceFields class-binding) (interpret-function-def statement (instanceMethods class-binding) next)))))

(define parentClassName car)
(define staticMethods cadr)
(define instanceFields caddr)
(define instanceMethods cadddr)
(define get-function car)
(define parent-class cddr)

;(var x) or (var x 3)
(define create-instance-field
  (lambda (statement environment className)
    (let ((class-binding (get-binding className environment)))
    (cond
      ((null? (parent-class statement))
       (update-binding className (list (parentClassName class-binding) (staticMethods class-binding) (create-binding (cadr statement) 'null environment) (instanceFields class-binding)) environment))
      (else
       (update-binding className (list (parentClassName class-binding) (staticMethods class-binding) (create-binding (cadr statement) (caddr statement) environment) (cadddr class-binding)) environment))))))
                                          
(define create-static-method
  (lambda (statement environment next className)
    (let ((class-binding (get-binding className environment)))
      (list (parentClassName class-binding) (interpret-function-def (get-function statement) (staticMethods class-binding) next) (instanceFields class-binding) (instanceMethods class-binding))))) 

(define true-type cadr)
(define instance-fields caddr)
;(new A)
(define create-instance-closure
  (lambda (statement environment)
    (let ((class-binding (get-binding (cadr statement) environment)))
    (list (true-type statement) (instance-fields class-binding)))))

;Returns the function that creates the function environment w/binding parameters
(define function-environment
  (lambda ()
    ((lambda (f)
       (f f))
     (lambda (a)
       (lambda (formal-params actual-params environment className)
         (cond
           ((and (not (null? actual-params)) (null? formal-params)) (myerror "Inproper number of variables"))
           ((null? formal-params) environment)
           ((eq? 'this (car formal-params)) (insert (car formal-params) (get-binding className environment)))  
           (else ((a a) (cdr formal-params) (cdr actual-params) (insert (car formal-params) (car actual-params) environment)))))))))

(define function-class
  (lambda ()
    ((lambda (f)
       (f f))
     (lambda (a)
       (lambda (environment)
         (car (get-binding 'this environment)))))))
  
       
(define function-name cadr)
(define get-func-environment caddr)
(define get-function-params car)
(define get-function-body cadr)
(define get-actual-params cddr)

(define actual-parameters-list
  (lambda (lis environment return throw next)
    (cond
      ((null? lis) lis)
      ((number? (car lis)) (cons (car lis) (actual-parameters-list (cdr lis) environment return throw next)))
      (else (cons (eval-expression (car lis) environment return throw next) (actual-parameters-list (cdr lis) environment return throw next)))))) ; is lookup the name I want here

(define get-binding
  (lambda (func-name environment)
    (lookup-variable func-name environment)))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw next className)
    (return (eval-expression (get-expr statement) environment return throw next className))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment return throw next className)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment return throw next className) environment))
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement environment return throw next className)
    (next (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment return throw next) environment))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw next className)
    (cond
      ((eval-expression (get-condition statement) environment return throw next className) (interpret-statement (get-then statement) environment return break continue throw next className))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw next))
      (else (next environment)))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw next)
    (letrec ((loop (lambda (condition body environment)
                     (if (eval-expression condition environment return throw next)
                         (interpret-statement body environment return (lambda (env) (next env)) (lambda (env) (loop condition body env)) throw (lambda (env) (loop condition body env)))
                         (next environment)))))
      (loop (get-condition statement) (get-body statement) environment))))

; Interprets a block.  The break, continue, throw and "next statement" continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw next)
    (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))
                                         (lambda (env) (next (pop-frame env))))))

; We use a continuation to throw the proper value.  Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment return throw next className)
    (throw (eval-expression (get-expr statement) environment return throw next className) environment)))

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
  (lambda (statement environment return break continue throw next)
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
  (lambda (expr environment return throw next className) ; ????????
    (cond
      ((number? expr) (return expr))
      ((eq? expr 'true) (return #t))
      ((eq? expr 'false) (return #f))
      ((not (list? expr)) (return (lookup expr environment)))
      ((eq? (expression-type expr) 'funcall) (eval-func-call expr environment (lambda (v) (return v)) throw next className))
      ((eq? (expression-type expr) 'new) (eval-instance expr environment))
      ((eq? (expression-type expr) 'dot) (eval-dot expr environment return throw next className))
      (else (eval-operator expr environment return throw next className)))))

(define expression-type car)

(define eval-dot
  (lambda (statement environmnet)
    (eval-expression ((cadddr (get-binding (caddr statement) environment)) environment))))

(define eval-instance
  (lambda (statement environment)
    (create-instance-closure statement environment)))
; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment return throw next className)
    (cond
      ((eq? '! (operator expr)) (return (not (eval-expression (operand1 expr) environment return throw next className))))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (return (- (eval-expression (operand1 expr) environment return throw next className))))
      (else (return (eval-binary-op2 expr (eval-expression (operand1 expr) environment return throw next className) environment return throw next className))))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment return throw next className)
    (cond
      ((eq? '+ (operator expr)) (return (+ op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '- (operator expr)) (return (- op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '* (operator expr)) (return (* op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '/ (operator expr)) (return (quotient op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '% (operator expr)) (return (remainder op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '== (operator expr)) (return (isequal op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '!= (operator expr)) (return (not (isequal op1value (eval-expression (operand2 expr) environment return throw next className)))))
      ((eq? '< (operator expr)) (return (< op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '> (operator expr)) (return (> op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '<= (operator expr)) (return (<= op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '>= (operator expr)) (return (>= op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '|| (operator expr)) (return (or op1value (eval-expression (operand2 expr) environment return throw next className))))
      ((eq? '&& (operator expr)) (return (and op1value (eval-expression (operand2 expr) environment return throw next className))))
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
      ((eq? var (car varlist)) (cons (scheme->language (box val)) (cdr vallist)))
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