#lang racket

;; Throw an error if no binding is found
(define throw-no-binding-error
  (lambda (lookup-sym)
    (error 'apply-env "No binding for ~s" lookup-sym)))

;; Throw an error if environment is invalid
(define throw-invalid-env
  (lambda (ctx)
    (error 'apply-env "Bad environment: ~s" ctx)))

;; Initialize an empty environment
(define init-env
  (lambda ()
    (list 'init-env)))

;; Attach a new binding to an environment
(define attach-env
  (lambda (ctx var val)
    (list 'attach-env var val ctx)))

;; Resolve a variable by searching the environment
(define resolve-env
  (lambda (ctx lookup-sym)
    (cond
      ((eqv? (car ctx) 'init-env)
       (throw-no-binding-error lookup-sym))
      ((eqv? (car ctx) 'attach-env)
       (let ((old-var (cadr ctx))
             (old-val (caddr ctx))
             (old-ctx (cadddr ctx)))
         (if (eqv? lookup-sym old-var)
             old-val
             (resolve-env old-ctx lookup-sym))))
      (else
       (throw-invalid-env ctx)))))

;; entry point
(define main-run
  (lambda (code)
    (let ((root-env (init-env)))
      (cond
        ((eqv? (car code) 'program)
         (process-statements (cdr code) root-env)
         (display "\nDone"))  ; matches original's final "Done" on its own line
        (else
         (display "This doesn't look like a program."))))))

;; Recursively process a list of statements in the given environment
(define process-statements
  (lambda (st-list env)
    (cond
      ((null? st-list) env)
      (else
       (process-statements (cdr st-list)
                           (interpret-statement (car st-list) env))))))

;; Interpret a single statement
(define interpret-statement
  (lambda (st env)
    (cond
      ((eqv? (car st) 'print)
       (begin
         (display "\n")
         (display (evaluate-expression env (cadr st)))
         env))
      ((eqv? (car st) '=)
       (attach-env env (cadr st) (evaluate-expression env (caddr st))))
      ((eqv? (car st) 'if)
       (let* ((cond-value (evaluate-expression env (cadr st)))
              (then-stmts (cadr (caddr st))))
         (if cond-value
             (process-statements then-stmts env)
             env)))
      ((eqv? (car st) 'while)
       (let* ((cond-exp (cadr st))
              (body-stmts (cadr (caddr st))))
         (let loop ((current-env env))
           (if (evaluate-expression current-env cond-exp)
               (loop (process-statements body-stmts current-env))
               current-env))))
      (else
       env))))

;; Evaluate an expression
(define evaluate-expression
  (lambda (env e)
    (cond
      ((integer? e) e)
      ((symbol? e) (resolve-env env e))
      ((list? e)
       (cond
         ((eqv? (car e) '+)
          (+ (evaluate-expression env (cadr e))
             (evaluate-expression env (caddr e))))
         ((eqv? (car e) '-)
          (- (evaluate-expression env (cadr e))
             (evaluate-expression env (caddr e))))
         ((eqv? (car e) '*)
          (* (evaluate-expression env (cadr e))
             (evaluate-expression env (caddr e))))
         ((eqv? (car e) '/)
          (/ (evaluate-expression env (cadr e))
             (evaluate-expression env (caddr e))))
         ((eqv? (car e) '<)
          (< (evaluate-expression env (cadr e))
             (evaluate-expression env (caddr e))))
         ((eqv? (car e) '>)
          (> (evaluate-expression env (cadr e))
             (evaluate-expression env (caddr e))))
         ((eqv? (car e) '&)
          (let ((v1 (evaluate-expression env (cadr e)))
                (v2 (evaluate-expression env (caddr e))))
            (if (and v1 v2) v2 #f)))
         ;; Unknown list expression → return 0 or #f silently:
         (else 0)))
      ;; Unknown atomic expression → return 0 or #f silently:
      (else 0))))
(define interpreter main-run)

      
;;; DON'T TOUCH THE LINE BELOW THIS ONE IF YOU WANT TO RECEIVE A GRADE! ;;;
(provide interpreter)
;;; DON'T TOUCH THE LINE ABOVE THIS ONE IF YOU WANT TO RECEIVE A GRADE! ;;;

;;; Sample testing programs below ;;;
;;; Actual tests NOT guaranteed match the tests below ;;;

; aprog tests simple print statement
; text file: print 1 + 2
(define aprog
  '( program ( print ( + 1 2 ) ) )) ; should print 3

; bprog tests input3.txt (should get error message - no dog variable)
(define bprog
  ' ( program ( = x ( * ( + 2 3 ) dog ) )
              ( print x ) ))

; cprog tests input2.txt
(define cprog
  '( program
     ( = x 3 )
     ( = y 5 )
     ( = z ( / ( + x y ) 3 ) )
     ( print z ) )) ; should print 8/3

; dprog tests input1.txt
(define dprog
 '( program ( = dog 1 )
            ( = x ( / ( + 2 3 ) 4 ) )
            ( = y ( / dog ( + x 3 ) ) )
            ( print ( + x y ) ) )) ; should print 101/68

; eprog tests input4.txt
(define eprog
  ' ( program ( print ( - ( - ( - 1 2 ) 3 ) 4 ) )            ; should print -8
              ( print ( / ( * ( / ( * 1 2 ) 3 ) 4 ) 5 ) ) )) ; should print 8/15

; fprog is a list representaion of a TINY program that tests if statements with logical operators
(define fprog
  '(program
    (= x 2)                                                       ; should print 2
    (if (& x 2) (then ((print x) (= x(- x 1)) (print x)) end ) ))) ; should print 1

; gprog is a list representaion of a TINY program that can handle while loops (statements)
(define gprog
  '(program
    (= x 10)
    (while (> x 0) (then ((print x)(= x (- x 1))) end) ))) ; should print numbers 10 - 1

; hprog is a list representaion of a TINY program that test if statements with relational operators
(define hprog
  '(program
    (= x 10)                                                      ; should print 10
    (if (> x 0) (then ((print x)(= x (- x 1)) (print x)) end) ))) ; should print 9

; iprog is a list representation of a TINY program that tests if statements with logical operators
(define iprog
  '(program
    (= x 5)
    (if (& x #f) (then ((print x) (= x(- x 1)) (print x)) end) ))) ; should not print anything

; How to call the interpreter for [x]prog
;(interpreter aprog)
