;; 2.54
(define (equal? l1 l2)
  (if (or (null? l2) (null? l1))
      (and (null? l2) (null? l1))
      (and
       (eq? (car l1) (car l2))
       (equal? (cdr l1) (cdr l2)))))


;; 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (deriv (base exp) var)
           (make-exponentiation
            (base exp)
            (make-sum (exponent exp) -1)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))
(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else (list '** base exponent))))

;; 2.57  ; doesnt actually work quite perfectly
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) ; dont need to change - never used with over 2 args
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (> (length s) 3)
      (cons `+ (cddr s))
      (caddr s)))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (if (> (length p) 3)
                             (cons `* (cddr p))
                             (caddr p)))


;; 2.58
;; a)
(define (in-list? s l)
  (cond ((null? l) #f)
        ((eq? s (car l)) #t)
        (else (in-list? s (cdr l)))))

(define (sum? x) (in-list? `+ x))
(define (product? x) (in-list? `* x))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))  ; only change this bit

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;; b)
(define (simplify exp)  ; simplifies a list of length 1 to just contained
  (if (null? (cdr exp)) (car exp) exp))

(define (nonmemq s xs)  ; takes first part until symbol s
  (if (eq? s (car xs))
      `()
      (cons (car xs) (nonmemq s (cdr xs)))))


(define (addend s) (simplify (nonmemq `+ s)))
(define (augend s) (simplify (cdr (memq `+ s))))

(define (multiplier s) (simplify (nonmemq `* s)))
(define (multiplicand s) (simplify (cdr (memq `* s))))
