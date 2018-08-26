;; 2.73
;; a) a call to operands will fail if its a variable or number

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (op-sum addend augend)
  (make-sum (deriv addend var)
            (deriv augend var)))

(define (op-product multiplier multiplicand)
  (make-sum
   (make-product multiplier (deriv multiplicand))
   (make-product (deriv multiplier) multiplicand)))

(define install-deriv
  (put 'deriv '+ op-sum)
  (put 'deriv '* op-product)
  'done)

;; d) just change the order in put



;; 2.74
(define (get-record file employee)
  (apply-generic 'get-record file employee))
;; should store access methods tagged with get-record

(define (get-salary file employee)
  (apply-generic 'get-salary (apply-generic 'get-record file employee)))

(define (find-employee-record files employee)
  (cond ((null? files) (error "no such employee in files"))
        ((has-record? (car files) employee) (get-record (car files) employee))
        (else (find-employee-record (cdr files) employee))))
; should return false if no employee in the file
; merger should add methods stored in table in same way


;; 2.75
(define (make-from-mag-ang mag ang)
  (define (dispatch
    (cond ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          ((eq? op 'real-part)
           (* mag (cos ang)))
          ((eq? op 'imag-part)
           (* mag (sin ang)))
          (else (error "UNKNOW OPERATION"))))
  dispatch)



;; 2.76
#|
New operations often: generic operations with explicit dispatch
New types often: message-passing style (OOP?)

New type:
message passing - just make a new type as whole thing at once
data-directed - add a new package to the table
generic operations - make a new type with special selectors

New operation:
message passing - add operation to each type individually
data-directed - new row to table with operation added to each package
generic operations - make new generic operation using existing selectors
|#



