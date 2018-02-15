; Jason Harnack, Tom Pescatore, Vicki Kelly
; EECS 345 Interpreter Project

; 1. Write a function called interpret
; 2. Funciton thatll store the states
; 3. Basic state evaluator
; 4. Returning a value
; 5. Create a parse tree
;   - if, variable declaration, assignment, return, while
; 6. Arithmetic evaluator 

(load "simpleParser.scm")

(define emptyState '(()()))

(define getFirst car)
(define getSecond cadr)
(define getThird caddr)
(define getRemaining cdr)
(define getVars car)
(define getValues cdr)

(define Interpret
  (lambda (fileName)
    (Evaluate (parser fileName) emptyState))) 

(define Evaluate
  (lambda (lis state)
    (cond
      ((null? lis) state)
      ((pair? (getRemaining lis)) (Evaluate (getRemaining lis) (SelectState (getFirst lis) state)))
      (else (SelectState (getFirst lis) state)))))
       
;car gets the symbols of all the statements
(define SelectState
  (lambda (stmt state)
    (cond
      ((eq? (car stmt) 'var) (Mvar stmt state))
      ((eq? (car stmt) '=) (Massign stmt state))
      ((eq? (car stmt) 'if) (Mif stmt state))
      ((eq? (car stmt) 'while) (Mwhile stmt state))
      ((eq? (car stmt) 'return) (Mreturn stmt state))
      (else (error "Unknown function")))))
      

(define Mvar
  (lambda (stmt state)
    (if (member (cadr stmt) (getVars state))
        (error "Variable already declared")
        (cons (append (getVars state) (list(cadr stmt))) (list (append '(()) (cadr state)))))))

;Stmt format (= variableName (expression or number) )
(define Massign
  (lambda (stmt state)
    (cond
      ((not (member (cadr stmt) (getVars state))) (error "Variable Not declared"))
      ((or (number? (getThird stmt)) (boolean? (getThird stmt))) (UpdateValue (getSecond stmt) (getThird stmt) state))
      ((not (list? (getThird stmt))) (UpdateValue (getSecond stmt) (GetVarValue (getThird stmt)) state))
      (else (EvaluateExpr (ReplaceVarsWithValues (getThird stmt)))))))



(define UpdateValue
  (lambda (varName newValue state)
    (cond
      ((eq? (caar state) varName) (list (car state) (cons newValue (cdadr state))))
      (else (list (cons (caar state) (car (UpdateValue varName newValue (list (cdar state) (cdadr state)))))
                  (cons (caadr state) (cadr (UpdateValue varName newValue (list (cdar state) (cdadr state))))))))))

(define EvaluateExpr
  (lambda (stmt state)
    (cond
      (else ()))))
      

(define ReplaceVarsWithValues
  (lambda (stmt state)
    (cond
      (()))))

        
;Recurses by loping off the first element of the vars and vals list
(define GetVarValue
  (lambda (varName state)
    (cond
      ((not (member varName (getVars state))) (error "Variable Not Declared"))
      ((eq? varName (caar state)) (if (null? (caadr state)) (error "variable not assigned") (caadr state)))
      (else (GetVarValue varName (list (cdar state) (cddr state)))))))
