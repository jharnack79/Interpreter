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
    (Evaluate (parser fileName) emptyState))) 

;Evaluates the parsed text file 
(define Evaluate 
  (lambda (lis state)
    (cond
      ((null? lis) state)
      ((pair? (getRemaining lis)) (Evaluate (getRemaining lis) (SelectState (getFirst lis) state)))
      (else (SelectState (getFirst lis) state)))))
       
;Handles all state expressions and evaluates them based on given operation
(define SelectState
  (lambda (stmt state)
    (cond
      ((eq? (getFirst stmt) 'var) (Mvar stmt state))
      ((eq? (getFirst stmt) '=) (Massign stmt state))
      ((eq? (getFirst stmt) 'if) (Mif stmt state))
      ((eq? (getFirst stmt) 'while) (Mwhile stmt state))
      ((eq? (getFirst stmt) 'return) (Mreturn stmt state))
      (else (error "Unknown function")))))
      
;Variable Declaration operation
;Throws an error if the value has been declared, otherwise it will step through the state until it reaches the end
;will then append new variable with a value of () to the current state
(define Mvar-cps
  (lambda (stmt state return)
    (cond
      ((member (getSecond stmt) (getVars state)) (return (error "Variable already declared")))
      ((pair? (cddr stmt)) (return (Massign (list '= (getSecond stmt) (getThird stmt)) (cons (append (getVars state) (list (cadr stmt))) (list (append (getValues state) '(()) ))))))
      (else (return (cons (append (getVars state) (list (cadr stmt))) (list (append (getValues state) '(()) ))))))))

(define Mvar
  (lambda (stmt state)
    (Mvar-cps stmt state (lambda (v) v))))

;Stmt format (= variableName (expression or number) )
;Assignment operation for declared variables
(define Massign-cps
  (lambda (stmt state return)
    (cond
      ((not (member (cadr stmt) (getVars state))) (return (error "Variable Not declared")))
      (else (return (UpdateValue (getSecond stmt) (M_value (getThird stmt) state) state))))))

(define Massign
  (lambda (stmt state)
    (Massign-cps stmt state (lambda (v) v))))

;Given a declared variable and a new value, the state is updated 
(define UpdateValue
  (lambda (varName newValue state)
    (cond
      ((eq? (getFirstVar state) varName) (list (getVars state) (cons newValue (cdadr state))))
      (else (list (cons (getFirstVar state) (car (UpdateValue varName newValue (getStateWithoutFirstPair state))))
                  (cons (getFirstValue state) (cadr (UpdateValue varName newValue (getStateWithoutFirstPair state)))))))))

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
(define Mif
  (lambda (stmt state)
    (Mif-cps stmt state (lambda (v) v))))

(define Mif-cps
  (lambda (stmt state return)
    (cond
      ((M_value (getSecond stmt) state) (return (Evaluate (list (getThird stmt)) state)))
      ((pair? (cdddr stmt)) (return (Evaluate (list (cadddr stmt)) state)))
      (else (return state)))))

;Takes while loop and evaluates given loop body if the while condition is true
(define Mwhile
  (lambda (stmt state)
    (Mwhile-cps stmt state (lambda (v) v))))

(define Mwhile-cps
  (lambda (stmt state return)
    (cond
      ((M_value (getSecond stmt) state) (return (Mwhile-cps stmt (Evaluate (list (getThird stmt)) state) (lambda (v) v))))
      (else (return state)))))