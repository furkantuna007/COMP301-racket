#lang eopl

;; data structures for let-lang.

(provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  
  (bool-val
   (boolean boolean?))
  
  ;; -----------------------
  ;; INSERT YOUR CODE HERE
  (str-val
   (str string?))

  (rational-val
   (ratval pair?))

  (pair-val (car expval?)
            (cdr expval?))
  

  (list-val
   (value list?))
    
   
  ;; -----------------------

  ;; -----------------------
)

;(define-dataype bool-val bool-val?
 ; (bool-val boolean?))
;;; extractors:
(define num-val?
  (lambda (exp)
    (cases expval exp
     (num-val (value) #t)
     (else #f))))

(define bool-val?
  (lambda (exp)
    (cases expval exp
     (bool-val (value) #t)
     (else #f))))

(define str-val?
  (lambda (exp)
    (cases expval exp
     (str-val (value) #t)
     (else #f))))

(define rational-val?
  (lambda (exp)
    (cases expval exp
     (rational-val (value) #t)
     (else #f))))


  



;; expval->num : ExpVal -> Int
;; Page: 70
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; -----------------------
;; INSERT YOUR CODE HERE

(define expval->rational
  (lambda (v)
    (cases expval v
      (rational-val (rational) rational)
      (num-val (num) num)
      (else (expval-extractor-error 'rational v)))))

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (list) list)
      
      (else (expval-extractor-error 'list v)))))
;; -----------------------

;; -----------------------

;; expval->bool : ExpVal -> Bool
;; Page: 70
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

(define empty-env-record
  (lambda () 
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-env-record? null?)

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))