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
(define getRemaining cdr)

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
      ((eq? (car stmt) 'var) (Mvar (stmt state)))
      ((eq? (car stmt) '=) (Massign (stmt state)))
      ((eq? (car stmt) 'if) (Mif (stmt state)))
      ((eq? (car stmt) 'while) (Mwhile (stmt state)))
      ((eq? (car stmt) 'return) (Mreturn (stmt state)))
      (else (error "Unknown function")))))
      

(define Mvar 
      