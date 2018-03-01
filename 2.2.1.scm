(define nil `())  ; not defined in MIT Scheme

;; --- 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
    lst
    (last-pair (cdr lst))))


;; --- 2.18
(define (revers xs)
  (if (null? xs) `()
    (append (revers (cdr xs)) (list (car xs)))))


;; --- 2.19
(define (first-denominaton coin-values) (car coin-values))
(define (except-first-denominaton coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))
; counts all possible combos: order irrelevant


;; --- 2.20
(define (same-parity head . xs)
  (cons head (filter
               (if (odd? head) odd? even?)
               xs)))


;; --- 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))


;; --- 2.23
(define (for-each f items)
  (cond ((null? items) #t)
        (else
          (f (car items))
          (for-each f (cdr items)))))
