; Jason Harnack, Tom Pescatore, Vicki Kelly
; EECS 345 Interpreter Project


(load "simpleParser.scm")

;Abstracting out our empty state
;State Design - ((variables) (values))
(define emptyState '(()()))

(define getFirst car)
(define getSecond cadr)
(define getThird caddr)
(define getRemaining cdr)
(define getVars car)
(define getValues cadr)
(define getFirstVar caar)
(define getFirstValue caadr)
(define getStateWithoutFirstPair
  (lambda (state)
    (list (cdar state) (cdadr state))))

;Main function to take filename and begin evaluating
(define Interpret
  (lambda (fileName)
    (Evaluate (parser fileName) emptyState (lambda (v) v)))) 

;Evaluates the parsed text file
(define Evaluate 
  (lambda (lis state return)
    (cond
      ((null? lis) (return state))
      ((pair? (getRemaining lis))  (SelectState (getFirst lis) state (lambda (v) (Evaluate (cdr lis) v return))))
      (else (SelectState (getFirst lis) state return)))))
       
;Handles all state expressions and evaluates them based on given operation
(define SelectState
  (lambda (stmt state return)
    (cond
      ((eq? (getFirst stmt) 'var) (Mvar-cps stmt state return))
      ((eq? (getFirst stmt) '=) (Massign-cps stmt state return))
      ((eq? (getFirst stmt) 'if) (Mif-cps stmt state return))
      ((eq? (getFirst stmt) 'while) (Mwhile-cps stmt state return))
      ((eq? (getFirst stmt) 'return) (Mreturn-cps stmt state return))
      ((eq? (getFirst stmt) 'begin) (Mbegin-cps stmt (cons emptyState state) return))
      ((eq? (getFirst stmt) 'break) (Mbreak state return))
      ((eq? (getFirst stmt) 'throw) (return (cadr stmt)))
      (else (error "Unknown function")))))

(define Mbegin-cps
  (lambda (stmt state return)
    (cond
      ((null? lis) (return (cdr state)))
      ((pair? (getRemaining lis)) (SelectState (getFirst lis) state (lambda (v) (Mbegin-cps (cdr lis) v return))))
      (else (SelectState (getFirst lis) state return)))))

(define Mbreak
  (lambda (state return)
    (cond
      ((null? (cdr state)) (error "Cannot Break on Base Level"))
      (else (return (cdr state))))))

;Variable Declaration operation
;Throws an error if the value has been declared, otherwise it will step through the state until it reaches the end
;will then append new variable with a value of () to the current state
;Need to abstract all the cars and cdrs
(define Mvar-cps ; THIS IS QUESTIONABLE RIGHT NOW - IF NOT WORKING REMOVE (CAR STATE) ON EVERYTHING
  (lambda (stmt state return)
    (cond
      ((isItAlreadyDeclared (getSecond stmt) (getVars state)) (return (error "Variable already declared")))
      ((pair? (cddr stmt)) (Massign-cps (list '= (getSecond stmt) (getThird stmt)) (cons (append (getVars (car state)) (list (cadr stmt))) (list (append (getValues (car state)) '(()) ))) return))
      (else (return (cons (append (getVars (car state)) (list (cadr stmt))) (list (append (getValues (car state)) '(()) ))))))))

(define isItAlreadyDeclared
  (lambda (varName state)
    (cond
      ((null? (car state)) #f) 
      ((atom? (caar state)) (list? (member varName (car state))))
      ((list? (member varName (caar state))) #t)
      (else (isItAlreadyDeclared varName (cdr state))))))

;Stmt format (= variableName (expression or number) )
;Assignment operation for declared variables
(define Massign-cps
  (lambda (stmt state return)
    (cond
      ((not (member (cadr stmt) (getVars state))) (return (error "Variable Not declared")))
      (else (M_value (getThird stmt) state (lambda (v) (UpdateValueState (getSecond stmt) v return)))))))
 

;Helper update function to iterate through each state layer in order to find the layer in which the variable exists
(define UpdateValueState
  (lambda (varName newValue state return)
    (cond
      ((or (null? (car state)) (atom? (caar state))) (UpdateValue varName newValue state return))
      ((not (member varName (caar state))) (cons (car state) (UpdateValueState varName newValue (cdr state) return)))
      (else (UpdateValue varName newValue (car state) (lambda (v) (return (cons v (cdr state)))))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;Given a declared variable and a new value, the state is updated 
(define UpdateValue
  (lambda (varName newValue state return)
    (cond
      ((eq? (getFirstVar state) varName) (return (list (getVars state) (cons newValue (cdadr state)))))
      (else (UpdateValue varName newValue (getStateWithoutFirstPair state)
                         (lambda (v1) (UpdateValue varName newValue (getStateWithoutFirstPair state)
                                                   (lamdba (v2) (return (list (cons (getFirstVar state) v1) (cons (getFirstValue state) v2)))))))))))
   

;Handles all potential arithmetic and boolean expression evaluations
(define M_value-cps
  (lambda (e state return)
    (cond
      ((number? e) (return e))
      ((boolean? e) (return e))
      ((eq? 'false e) (return #f))
      ((eq? 'true e) (return #t))
      ((not (list? e)) (return (GetVarValue e state)))
      ((eq? '+ (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (+ v1 v2)))))))
      ((eq? '* (operator e)) ((M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (* v1 v2)))))))
      ((and (pair? (cddr e)) (eq? '- (operator e))) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (- v1 v2)))))))
      ((eq? '- (operator e)) (M_value-cps (operand1 e) state (lambda (v) (return (* -1 v)))))
      ((eq? '/ (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (quotient v1 v2)))))))
      ((eq? '% (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (remainder v1 v2)))))))
      ((eq? '&& (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (and v1 v2)))))))
      ((eq? '= (operator e)) (return (GetVarValue (operand1 e) (Massign e state))))
      ((eq? '|| (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (or v1 v2)))))))
      ((eq? '== (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (eq? v1 v2)))))))
      ((eq? '> (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (> v1 v2)))))))
      ((eq? '< (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (< v1 v2)))))))
      ((eq? '>= (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (>= v1 v2)))))))
      ((eq? '<= (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (<= v1 v2)))))))
      ((eq? '!= (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (not (eq? v1 v2))))))))
      ((eq? '! (operator e)) (M_value-cps (operand1 e) state (lambda (v) (return (not v)))))             
      (else (return (error 'badop "Undefined operator")))))))

(define M_value
  (lambda (e state)
    (M_value-cps e state (lambda (v) v))))

; helper functions to easily extract the operands and operators from a statement
(define operator
  (lambda (e)
    (car e)))

(define operand1
  (lambda (e)
    (cadr e)))
 
(define operand2
  (lambda (e)
    (caddr e)))

;Helper function to iterate through each state until it finds the state that has the declared variable and its value
(define GetVarValueState
  (lambda (varName state)
    (cond
      ((or (null? (car state)) (atom? (caar state))) (GetVarValue varName state))
      ((not (member varName (caar state))) (GetVarValueState varName (cdr state)))
      (else (GetVarValue varName (car state))))))
        
;Recurses by loping off the first element of the vars and vals list
(define GetVarValue
  (lambda (varName state)
    (cond
      ((not (member varName (getVars state))) (error "Variable Not Declared"))
      ((eq? varName (caar state)) (if (null? (caadr state)) (error "variable not assigned") (caadr state)))
      (else (GetVarValue varName (getStateWithoutFirstPair state))))))

;Returns the correct value when broken down to either a number, boolean, or variable
(define Mreturn
  (lambda (stmt state)
    (Mreturn-cps stmt state (lambda (v) v))))

(define Mreturn-cps
  (lambda (stmt state return)
    (cond
      ((eq? (M_value (cadr stmt) state) #t) (return 'True))
      ((eq? (M_value (cadr stmt) state) #f) (return 'False))
      (else (return (M_value (cadr stmt) state))))))

;Takes the if statement and assess the expression after it,
;Depending on the result, it will execute either the then or else statement associated
(define Mif-cps
  (lambda (stmt state return)
    (cond
      ((M_value (getSecond stmt) state) (Evaluate (list (getThird stmt)) state return))
      ((pair? (cdddr stmt)) (Evaluate (list (cadddr stmt)) state return))
      (else (return state)))))

;Takes while loop and evaluates given loop body if the while condition is true
(define Mwhile-cps
  (lambda (stmt state return)
    (cond
      ((M_value (getSecond stmt) state) (Evaluate (list (getThird stmt)) state (lambda (v) (Mwhile-cps stmt v return))))
      (else (return state)))))