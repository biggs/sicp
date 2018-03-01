;; --- 2.27
(define nil `())

(define (deep-reverse tree)
  (if (not (pair? tree))
    tree
    (append (deep-reverse (cdr tree))
            (list (deep-reverse (car tree))))))


;; --- 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))


;; --- 2.29
(define (make-mobile left right)
  (list left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch) (car branch))
(define (branch-structure branch)  ;; if just a weight, return as no not list
  (if (null? (cddr branch))
    (cadr branch)
    (cdr branch)))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (number? structure)
      structure
      (total-weight structure))))

(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (if (number? mobile)
    #t
    (and
      (=
        (* (branch-length (left-branch mobile))
           (branch-weight (left-branch mobile)))
        (* (branch-length (right-branch mobile))
           (branch-weight (right-branch mobile))))
      (balanced? (branch-structure (left-branch mobile)))
      (balanced? (branch-structure (right-branch mobile))))))

; new defs just (hardly changed - this is actually better implement!)
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))



;; --- 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree1 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree1 sub-tree)
           (square sub-tree)))
       tree))


;; --- 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       tree))

(define (square-tree2 tree) (tree-map square tree))



;; --- 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))
; subsets of A:B:...:() is always subsets(B:...:()) ++ A:subsets(B:...:())
; so just take the subsets of the set without first element and add to
; exactly the same subsets with first element added to them.
