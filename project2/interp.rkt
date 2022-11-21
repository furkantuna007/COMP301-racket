#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define gcd
(lambda (num1 num2)
        
 (if (zero? num1) num2 (if (zero? num2) num1 (if (zero? (- num1 num2)) num1 (if (> num1 num2) (gcd (- num1 num2) num2) (gcd num1 (- num2 num1))))))
))

(define gcd-logn
  (lambda (num1 num2)

     (if (= num2 0)
         num1
         (gcd-logn num2 (modulo num1 num2)))))



    

(define sum-helper
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum-helper (cdr lst))))))
    




;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                                    ;; -----------------------
                                    ;; INSERT YOUR CODE HERE
                              
                                    ;; -----------------------

                              ((= op 3) (/ num1 num2))
                               
                              (else (- num1 num2))
                              )))
                                    ;; -----------------------
                              
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons  (* num2bot num1) num2top))
                         (else  (cons (- (* num2bot num1) num2top) num2bot))))))

                              ;; -----------------------

                              
                              

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons num1top (* num1bot num2)))
                              (else  (cons (- num1top (* num1bot num2)) num1bot))))))
                              ;; -----------------------
                              

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                          
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                             ((= op 3)  (cons (* num1top num2bot) (* num1bot num2top)))
                             (else  (cons (- (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot)) )))))
                        ))))
              
                              ;; ----------------------- 
                            
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->rational val1)))
                     (if (number? num1)
                        (if (zero? num1)
                          (bool-val #t)
                          (bool-val #f))
                          ;; -----------------------
                          ;; INSERT YOUR CODE HERE
                          (if (= (car num1) 0)
                              (bool-val #t)
                              (bool-val #f))
                                  

                        )
                          ;; -----------------------


                          ;; ----------------------- 
                        )))

    

      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      

      ;; -----------------------
      ;; INSERT YOUR CODE HERE
      (rational-exp (num1 num2)
                            (if (= num2 0)
                                (eopl:error 'divide-by-zero-error "error")
                                (rational-val (cons num1 num2))))
      ;; -----------------------
     (if-exp (exp1 exp2 exp3 exp4 exp5)
            (let (( val1 (value-of exp1 env)))
              (cond
                ((expval->bool val1) (value-of exp2 env))
                ((null? exp3) (value-of exp5 env))
                ((null? (cdr exp3)) (value-of exp5 env))
                (else (value-of (if-exp (cadr exp3) (cadr exp4) (cddr exp3) (cddr exp4) exp5) env)))))

     
      (simpl-exp (exp)
               (let ((val1 (value-of exp env)))
                 (let ((num1 (expval->rational val1)))
                   (cond
                   ((number? num1) num1)
                   
                   (
                    (rational-val
                     (let ((num1top (car num1))
                        (num1bot (cdr num1)))
                     (cons (/ num1top (gcd-logn num1top num1bot)) (/ num1bot (gcd-logn num1top num1bot))))))
                 ))))

      
      (list-exp ()
               (list-val
                 '()))

      (cons-exp (exp1 lst)

                
                (let ((val1 (value-of exp1 env))
                     (val2 (value-of lst env)))
                  
                      
                      (list-val
                      (let ((num1 (expval->num val1)))
                        (let (( lst1 (expval->list val2)))
                        
                       
                          (cons num1 lst1))))))

      (sum-exp (lst)

                
               (let ((val1 (value-of lst env)))
                     (let ((lst1 (expval->list val1)))

                       (num-val
                       (sum-helper lst1))))
                    

               
               )
                        
                

      

       
       
      

    
      )))

      ;; -----------------------

     