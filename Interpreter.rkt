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
(define getValues cadr)
(define getFirstVar caar)
(define getFirstValue caadr)
(define getStateWithoutFirstPair
  (lambda (state)
    (list (cdar state) (cdadr state))))

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
    (cond
      ((member (getSecond stmt) (getVars state)) (error "Variable already declared"))
      ((pair? (cddr stmt)) (Massign (list '= (getSecond stmt) (getThird stmt)) (cons (append (getVars state) (list (cadr stmt))) (list (append (getValues state) '(()) )))))
      (else (cons (append (getVars state) (list (cadr stmt))) (list (append (getValues state) '(()) )))))))

;Stmt format (= variableName (expression or number) )
(define Massign
  (lambda (stmt state)
    (cond
      ((not (member (cadr stmt) (getVars state))) (error "Variable Not declared"))
      (else (UpdateValue (getSecond stmt) (M_value (getThird stmt) state) state)))))

(define UpdateValue
  (lambda (varName newValue state)
    (cond
      ((eq? (getFirstVar state) varName) (list (getVars state) (cons newValue (cdadr state))))
      (else (list (cons (getFirstVar state) (car (UpdateValue varName newValue (getStateWithoutFirstPair state))))
                  (cons (getFirstValue state) (cadr (UpdateValue varName newValue (getStateWithoutFirstPair state)))))))))

(define M_value
  (lambda (e state)
    (cond
      ((number? e) e)
      ((boolean? e) e)
      ((not (list? e)) (GetVarValue e state))
      ((eq? '+ (operator e)) (+ (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '* (operator e)) (* (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '- (operator e)) (- (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '/ (operator e)) (quotient (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '% (operator e)) (remainder (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '&& (operator e)) (and (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ;((eq? '= (operator e)) (GetVarValue (operand1 e) (Massign e state)))
      ((eq? '|| (operator e)) (or (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '== (operator e)) (eq? (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '> (operator e)) (> (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '< (operator e)) (< (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '>= (operator e)) (>= (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '<= (operator e)) (<= (M_value (operand1 e) state) (M_value (operand2 e) state)))
      ((eq? '!= (operator e)) (not (eq? (M_value (operand1 e) state) (M_value (operand2 e) state))))
      ((eq? '! (operator e)) (not (M_value (operand1 e) state)))             
      (else (error 'badop "Undefined operator")))))

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

(define Mreturn
  (lambda (stmt state)
    (M_value (cadr stmt) state)))
  
(define Mif
  (lambda (stmt state)
    (cond
      ((M_value (getSecond stmt) state) (Evaluate (list (getThird stmt)) state))
      ((pair? (cdddr stmt)) (Evaluate (list (cadddr stmt)) state))
      (else state))))
       