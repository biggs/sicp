;; -- 2.7 - 2.
(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (cdr x) (car x)))
(define (lower-bound x) (min (cdr x) (car x)))

(define (sub-interval a b)
  (make-interval (- (upper-bound a) (lower-bound b))
                 (- (lower-bound a) (upper-bound b))))

(define (div-interval x y)
  (if (& (< 0 upper-bound) (> 0 lower-bound))
    (mul-interval x
                  (make-interval
                    (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y))))
    (print "error")))

; 2.11 is long! but divide into +ve, -ve and zero spanning intervals
; and only need two mults when both span zero

(define (make-center-percent center percent)
  (let ((half-tolerance (\ 2 (* center percent))))
    (cons (+ center half-tolerence) (- center half-tolerence))))

(define (percent interval)
  (let ((ub (upper-bound interval)) (lb (lower-bound interval)))
    (\ (- ub lb) (\ 2 (+ ub lb)))))







;; --- 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(add-1 zero)






;; --- 2.5
(define (cons x y) (* (expt 2 x) (expt 3 y)))

(define (p-into-z p z)	; how many times p divides z
  (if (= 1 (gcd p z))	; 1 if doesnt divide
    0
    (+ 1 (p-into-z p (/ z p)))))

(define (car z) (p-into-z 2 z))
(define (cdr z) (p-into-z 3 z))

;F+&7
;plus ampersand



;; --- 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))


(car (cons x y)) ; substitution
(car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)


(define (cdr z)
  (z (lambda (p q) q)))



;; --- 2.3
(define (perim rect) ((* 2 (+ height width)) rect))
(define (area rect) ((+ height width) rect))

(define (make-rect bottom-left-corner top-right-corner)
  make-segment bottom-left-corner top-right-corner)
(define (height rect) (- (y-point (end-segment rect))
                          (y-point (start-segment rect))))




;; --- 2.2
(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (define (average a b) (* 0.5 (+ a b)))
  (let
    ((x1 (x-point (start-segment seg)))
     (y1 (y-point (start-segment seg)))
     (x2 (x-point (end-segment seg)))
     (y2 (y-point (end-segment seg))))
    (make-point (average x1 x2) (average y1 y2))))
