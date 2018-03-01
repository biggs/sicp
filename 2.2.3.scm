(define nil `())
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; --- 2.33
(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length1 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;; --- 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))


;; --- 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                                (count-leaves sub-tree)
                                1))
                   t)))


;; --- 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             nil
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))


;; --- 2. 37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col)
                  (dot-product m-row n-col))
                n-cols))
         m)))

; or better: use that m_ik.n_kj = nT_jk * m_(i)k where m_(i) are rows
(define (matrix-*-matrix m n)
  (map (lambda (m-row) (matrix-*-vector (transpose n) m-row)) m))



;; --- 2.39
(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))




;; --- 2.40
(define (enumerate-interval start end)
  (if (< end start)
    nil
    (cons start (enumerate-interval (+ 1 start) end))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
    (lambda (i) (map
                  (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))



;; --- 2.41
(define (ordered-triples n)
  (flatmap
    (lambda (i) (flatmap
                  (lambda (j) (map
                                (lambda (k) (list i j k))
                                (enumerate-interval 1 j)))
                  (enumerate-interval 1 i)))
    (enumerate-interval 1 n)))


(define (ord-trip-sum-s s n)
  (define (triple-sums-to-s? triple)
    (= s (+ (car triple) (cadr triple) (caddr triple))))
  (filter triple-sums-to-s? (ordered-triples n)))


;; --- 2.42
; implement as list length k of queen positins, k<=n, each value 0<i<=n
; RHS is first queen placed - i.e filling up board from RHS
(define empty-board `())
(define (adjoin-position new-row k rest-of-queens) (cons new-row rest-of-queens))

(define (safe? k positions)
  (define unsafe-positions
    (append
      (cdr positions)  ; horizontal and then diagonals
      (map + (cdr positions) (enumerate-interval 1 (- k 1)))
      (map - (cdr positions) (enumerate-interval 1 (- k 1)))))
  (null? (filter  ; simply check if new position is unsafe
           (lambda (unsafe-pos) (= (car positions) unsafe-pos))
           unsafe-positions)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))



;; --- 2.43
;; it recursively calls queens board-size times for each flatmap,
;; i.e. n^n times more than the original. (have seen several differnt ans!)
;; but certainly O(?^n)
