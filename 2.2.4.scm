;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; 2.45
(define (split sp1 sp2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split sp1 sp2) painter (- n 1))))
          (sp1 painter (sp2 smaller smaller))))))


;; 2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect u v)
  (make-vect
   (+ (xcor-vect u) (xcor-vect v))
   (+ (ycor-vect u) (ycor-vect v))))

(define (sub-vect u v)
  (make-vect
   (- (xcor-vect u) (xcor-vect v))
   (- (ycor-vect u) (ycor-vect v))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))


;; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

;; or
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2))(define (below1 painter1 ))
)
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge1-frame frame) (cddr frame))



;; 2.48
(define (make-segment origin-vect length-vect) (cons origin-vect length-vect))
(define (start-segment segment) (car segment))
(define (end-segment segment) (add-vect (car segment) (cdr segment)))


;; 2.49
(define outline-frame-segments
  `((make-segment (make-vector 0 0) (make-vector 0 1))
    (make-segment (make-vector 0 0) (make-vector 1 0))
    (make-segment (make-vector 1 0) (make-vector 1 1))
    (make-segment (make-vector 0 1) (make-vector 1 1))))

(define x-frame-segments
  `((make-segment (make-vector 0 0) (make-vector 1 1))
    (make-segment (make-vector 0 1) (make-vector 1 0))))

(define diamond-frame-segments
  `((make-segment (make-vector 0 0.5) (make-vector 0.5 1))
    (make-segment (make-vector 0 0.5) (make-vector 0.5 0))
    (make-segment (make-vector 0.5 0) (make-vector 0 0.5))
    (make-segment (make-vector 0.5 0) (make-vector 1 0.5))))

(define outline-painter (segments->painter outline-frame-segments))
(define diamond-painter (segments->painter diamond-frame-segments))
(define x-painter (segments->painter x-frame-segments))



;; 2.50
(define (flip-horiz painter)
  (transform-painter painter
                      (make-vect 1 0)
                      (make-vect 0 0)
                      (make-vect 1 1)))

(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))


;; 2.51
(define (below painter1 painter2)
  (let ((paint-bottom
         (transform-painter painter1
                            (make-vect 0 0)
                            (make-vect 1 0)
                            (make-vect 0 0.5))
         (paint-top
          (transform-painter painter2
                             (make-vect 0 0.5)
                             (make-vect 1 0.5)
                             (make-vect 0 1)))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))


;; 2.52
;; a) just add a couple of make-segments to the list
;; b) etc 
