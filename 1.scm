











(define (iterative-improve good-enough? improve)
  (lambda (x)
    (let ((xim (improve x)))
      (if (good-enough? x xim)
        xim
        ((iterative-improve good-enough? improve) x)))))


(define (sqrt y)
  ((iterative-improve
    (lambda (x xim) (< (abs (- x xim)) 0.01))
    (lambda (x) (average x (/ x y)))) 1.0)




(define (compose f g)
  (lambda (x) (f (g x)))
)


(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))
  )
)


(define (average a b) (* 0.5 (+ a b)))

(define (smooth f dx)
  (lambda (x) 
    (average
      (f (- x dx))
      (f (+ x dx))
    )
  )
)

((repeated (lambda (f) (smooth f 0.1)) 5) (lambda (x) (* x x)))







#| 
;; two versions of accululate

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))


(define (accumulate combiner null-value term a next b)
  (define (iter a result)  
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))
    )
  )
  (iter a null-value)
)









(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))



(define (calc-pi n)
  (define (up-2 x) (+ 2 x))
  (define (square x) (* x x))


  (* 8.0 n (/ (product square 4 up-2 n) (product square 3 up-2 (+ n 1))))

)




;; recursive and iterative calc
(define (f n) (cond
        ((< n 3) n)
        (else (+ (f (- n 1))
                  (* 2 (f (- n 2)))
                  (* 3 (f (- n 3)))))))
        

(define (g n) 
    (define (g-it a b c count)
        (if (< count 3)
            a
            (g-it (+ a (* 2 b) (* 3 c)) a b (- count 1))
        )
    )
    (g-it 2 1 0 n))



;; pascal triangle r(ow) c(ollumn)
(define (pas r c) 
    (cond
        ((= c 1) 1)
        ((= c r) 1)
        (else (+ (pas (- r 1) c) (pas (- r 1) (- c 1))))
    )
)



;; fast iterative exponentation
(define (fexp b n)
    (define (fexp-iter a b n)
        (cond
            ((= n 1) (* a b))
            ((even? n) (fexp-iter a (* b b) (/ n 2)))
            (else (fexp-iter (* a b) b (- n 1)))
        )
    )
(fexp-iter 1 b n))




;; multiplication using doubling, halving and addition only (iterative)
(define (fast-mult x y)

    (define (double n) (* n 2))
    (define (halve n) (/ n 2))

    (define (fast-mult-it a b n)
        (cond ((= n 1) (+ a b))
              ((even? n) (fast-mult-it a (double b) (halve n)))
              (else (fast-mult-it (+ a b) b (- n 1)))
        )
    )
    
    (fast-mult-it 0 x y)
)



;; fast fibonnachi (log time)
(define (fib n)
    (define (fib-iter a b p q count)
      (cond ((= count 0) 
             b)
            ((even? count)
             (fib-iter a
                       b
                       (+ (* p p) (* q q))
                       (+ (* 2 p q) (* q q))
                       (/ count 2)))
            (else 
             (fib-iter (+ (* b q) 
                          (* a q) 
                          (* a p))
                       (+ (* b p) 
                          (* a q))
                       p
                       q
                       (- count 1)))))

      (fib-iter 1 0 0 1 n))


(display 23) |#