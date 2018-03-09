; Jason Harnack, Tom Pescatore, Vicki Kelly
; EECS 345 Interpreter Project


(load "simpleParser.scm")

;Abstracting out our empty state
;State Design - ((variables) (values))
(define emptyState '(()()))
(define startingState '((() ())))

(define getFirst car)
(define getSecond cadr)
(define getThird caddr)

;Main function to take filename and begin evaluating
(define Interpret
  (lambda (fileName)
    (call/cc
     (lambda (break)
       (Evaluate (parser fileName) startingState (lambda (v) v) break)))))

;Evaluates the parsed text file
(define Evaluate 
  (lambda (lis state return break)
    (cond
      ((null? lis) (return state))
      ((pair? (getRest lis))  (SelectState (getFirst lis) state (lambda (v) (Evaluate (cdr lis) v return break)) break))
      (else (SelectState (getFirst lis) state return break)))))
       
;Handles all state expressions and evaluates them based on given operation
(define SelectState
  (lambda (stmt state return break)
    (cond
      ((eq? (getFirst stmt) 'var) (Mvar (getRest stmt) state return))
      ((eq? (getFirst stmt) '=) (Massign (getSecond stmt) (getThird stmt) state return))
      ((eq? (getFirst stmt) 'if) (Mif-cps stmt state return break))
      ((eq? (getFirst stmt) 'while) (Mwhile-cps stmt state return break))
      ((eq? (getFirst stmt) 'return) (Mreturn-cps (getRest stmt) state return break))
      ((eq? (getFirst stmt) 'begin) (Mbegin-cps (getRest stmt) (addLayer state) return break))
      ((eq? (getFirst stmt) 'break) (Mbreak state return))
      ((eq? (getFirst stmt) 'throw) (return (cadr stmt)))
      (else (error "Unknown function or function used in inappropriate place")))))

(define Mbegin-cps
  (lambda (stmt state return break)
    (cond
      ((or (null? stmt) (eq? (getFirst (getFirst stmt)) 'continue)) (return (removeLayer state)))
      (else (SelectState (getFirst stmt) state (lambda (v) (Mbegin-cps (getRest stmt) v return break)) break)))))

(define Mbreak
  (lambda (state return)
    (cond
      ((null? (cdr state)) (error "Cannot Break on Base Level"))
      (else (return (cdr state))))))

;Variable Declaration operation
;varAndMaybeValue will be either (var) or (var val) depending on wether or not the variable is being assigned a value at the same time as its declaration
(define Mvar
  (lambda (varAndMaybeValue state return)
    (cond
      ((pair? (getRest varAndMaybeValue)) (declareVar-cps (getFirst varAndMaybeValue) state (lambda (v) (Massign (getFirst varAndMaybeValue) (getSecond varAndMaybeValue) v return))))
      (else (declareVar-cps (getFirst varAndMaybeValue) state return)))))


;Stmt format (= variableName (expression or number) )
;Assignment operation for declared variables
(define Massign
  (lambda (var val state return)
    (M_value-cps val state (lambda (v) (assignVarValue-cps var v state return)))))
   

;Handles all potential arithmetic and boolean expression evaluations
(define M_value-cps
  (lambda (e state return)
    (cond
      ((number? e) (return e))
      ((not (list? e)) (getVarVal-cps e state return))
      ((eq? '+ (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (+ v1 v2)))))))
      ((eq? '* (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (* v1 v2)))))))
      ((and (pair? (cddr e)) (eq? '- (operator e))) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (- v1 v2)))))))
      ((eq? '- (operator e)) (M_value-cps (operand1 e) state (lambda (v) (return (* -1 v)))))
      ((eq? '/ (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (quotient v1 v2)))))))
      ((eq? '% (operator e)) (M_value-cps (operand1 e) state (lambda (v1) (M_value-cps (operand2 e) state (lambda (v2) (return (remainder v1 v2)))))))
      ((eq? '= (operator e)) (return (GetVarValue (operand1 e) (Massign e state))))
      (else (return (error 'badop "Undefined operator"))))))

(define intOperators '(+ * - / % =))

(define M_value_boolean-cps
  (lambda (e state return)
    (cond
      ((boolean? e) (return e))
      ((eq? 'false e) (return #f))
      ((eq? 'true e) (return #t))
      ((number? e) (return e))
      ((not (list? e)) (getVarVal-cps e state return))
      ((eq? '&& (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v1) (M_value_boolean-cps (operand2 e) state (lambda (v2) (return (and v1 v2)))))))
      ((eq? '|| (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v1) (M_value_boolean-cps (operand2 e) state (lambda (v2) (return (or v1 v2)))))))
      ((eq? '== (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v1) (M_value_boolean-cps (operand2 e) state (lambda (v2) (return (eq? v1 v2)))))))
      ((eq? '> (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v1) (M_value_boolean-cps (operand2 e) state (lambda (v2) (return (> v1 v2)))))))
      ((eq? '< (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v1) (M_value_boolean-cps (operand2 e) state (lambda (v2) (return (< v1 v2)))))))
      ((eq? '>= (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v1) (M_value_boolean-cps (operand2 e) state (lambda (v2) (return (>= v1 v2)))))))
      ((eq? '<= (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v1) (M_value_boolean-cps (operand2 e) state (lambda (v2) (return (<= v1 v2)))))))
      ((eq? '!= (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v1) (M_value_boolean-cps (operand2 e) state (lambda (v2) (return (not (eq? v1 v2))))))))
      ((eq? '! (operator e)) (M_value_boolean-cps (operand1 e) state (lambda (v) (return (not v)))))
      (else (return (error 'badop "Undefined operator"))))))

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

;Returns the correct value when broken down to either a number, boolean, or variable
(define Mreturn-cps
  (lambda (stmt state return break)
    (cond
      ((number? (getFirst stmt)) (break (getFirst stmt)))
      ((eq? (getFirst stmt) #f) (break 'False))
      ((eq? (getFirst stmt) #t) (break 'True))
      ((not (pair? (getFirst stmt))) (getVarVal-cps (getFirst stmt) state (lambda (v) (return (break v)))))
      ((member (getFirst (getFirst stmt)) intOperators) (M_value-cps (getFirst stmt) state (lambda (v) (return (break v)))))
      (else (M_value_boolean-cps (getFirst stmt) state (lambda (v) (return (break v))))))))

;Takes the if statement and assess the expression after it,
;Depending on the result, it will execute either the then or else statement associated
(define Mif-cps
  (lambda (stmt state return break)
    (M_value_boolean-cps (getSecond stmt) state (lambda (b) (if b
                                                                (Evaluate (list (getThird stmt)) state return break)
                                                                (if (pair? (cdddr stmt))
                                                                    (Evaluate (list (cadddr stmt)) state return break)
                                                                    (return state)))))))

;Takes while loop and evaluates given loop body if the while condition is true
(define Mwhile-cps
  (lambda (stmt state return break)
    (M_value_boolean-cps (getSecond stmt) state (lambda (b) (if b
                                                                (Evaluate (list (getThird stmt)) state (lambda (v) (Mwhile-cps stmt v return break)) break)
                                                                (return state))))))

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;State functions

(define getRest cdr)
(define getVarsOfLayer car)
(define getValuesOfLayer cadr)
(define getFirstVarOfLayer caar)
(define getFirstValueOfLayer caadr)

(define getStateWithoutFirstPair
  (lambda (state)
    (list (cdar state) (cdadr state))))

(define addLayer
  (lambda (state)
    (cons emptyState state)))

(define getFirstLayer
  (lambda (state)
    (car state)))

(define removeLayer
  (lambda (state)
    (cdr state)))

(define getVarVal-cps
  (lambda (var state return)
    (cond
      ((null? state) (error "Variable not declared"))
      (else (getVarInLayer-cps var (getFirstLayer state) (lambda (exists value) (if exists
                                                                                    (return value)
                                                                                    (getVarVal-cps var (removeLayer state) return))))))))

(define getVarInLayer-cps
  (lambda (var layer return)
    (cond
      ((null? (getVarsOfLayer layer)) (return #f #f))
      ((eq? (getFirstVarOfLayer layer) var) (if (eq? '() (getFirstValueOfLayer layer))
                                                (error "Variable not assigned a value")
                                                (return #t (getFirstValueOfLayer layer))))
      (else (getVarInLayer-cps var (getStateWithoutFirstPair layer) return)))))

(define isVarDeclared
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((member var (getVarsOfLayer (getFirstLayer state))) #t)
      (else (isVarDeclared var (removeLayer state))))))

(define declareVar-cps
  (lambda (var state return)
    (cond
      ((isVarDeclared var state) (error "Variable already declared"))
      (else (return (cons (addVarToLayer var (getFirstLayer state)) (removeLayer state)))))))

(define addVarToLayer
  (lambda (var layer)
    (list (append (getVarsOfLayer layer) (list var)) (append (getValuesOfLayer layer) '(())))))

(define assignVarValue-cps
  (lambda (var val state return)
    (cond
      ((null? state) (error "Variable not declared"))
      ((member var (getVarsOfLayer (getFirstLayer state))) (assignValueToVarInLayer var val (getFirstLayer state) (lambda (v) (return (cons v (removeLayer state))))))
      (else (assignVarValue-cps var val (removeLayer state) (lambda (v) (return (cons (getFirstLayer state) v))))))))

(define assignValueToVarInLayer
  (lambda (var val layer return)
    (cond
      ((eq? (getFirstVarOfLayer layer) var) (return (list (getVarsOfLayer layer) (cons val (getRest (getValuesOfLayer layer))))))
      (else (assignValueToVarInLayer var val (getStateWithoutFirstPair layer) (lambda (v) (return (list (cons (getFirstVarOfLayer layer) (getVarsOfLayer v)) (cons (getFirstValueOfLayer layer) (getValuesOfLayer v))))))))))