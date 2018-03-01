;; 2.59
(define (union-set s1 s2)
  (fold-right adjoin-set s2 s1))

;; 2.60
;; element-of-set and intersection-set are the same, and same O(n), O(n^2)
(define (adjoin-set x s) (cons x s)) ; now O(1)
;; union-set is same as above but now O(length(s1)) (just to get end of list)
;; on average, will use more space. good if many unique elements



;; 2.61
(define (adjoin-set x s)
  (cond ((null? s) x)
        ((= x (car s)) s)
        ((> x (car s)) (cons (car s) (adjoin-set x (cdr s))))
        (else (cons x s))))

;; 2.62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((x1 (car s1)) (x2 (car s2)))
                (cond ((= x1 x2)
                       (cons x1
                             (union-set (cdr s1) (cdr s2))))
                      ((< x1 x2)
                       (cons x1
                             (union-set (cdr s1) s2)))
                      (else
                       (cons x2
                             (union-set s1 (cdr s2)))))))))

;; 2.63
#|
In Haskell pseudo-code:

tree->list-1 [] = []
tree->list-1 tree = (tree->list-1 left-branch) ++ entry:(tree->list-1 right-branch)

copy-to-list [] result-list = result-list
copy-to-list tree result-list = copy-to-list left-branch entry:(copy-to-list right-branch result-list)
tree->list-2 tree = copy-to-list tree []

First version, T(n) = T(n/2) + O(n/2)
O(n/2) append each step, on two sub trees: O(n/2) + (2*O(n/4) + (4*O(n/8)) + ... + (n*O(1)) = no_terms * O(n/2) = O(n log n)
Second, rhs folds in first so only uses cons in each entry, no append: T(n) = 2T(n/2) + O(1)
so overall O(1 + 2 + 4 + ... n) = O(2*n) = O(n)
|#


;; 2.64
#|
a) It creates a tree of MEDIAN=this-entry and sub-trees either side of recursive application
b) T(n) = 2T(n/2) + O(1) ; the last bit is make-tree. Overall O(n) as above
|#


;; 2.65
(define (union-set-tree s1 s2)
  (list->tree (union-set (tree->list s1) (tree->list s2)))) ; O(n) + O(n) + O(n) = O(n)

(define (intersection-set-tree s1 s2)
  (list->tree (intersection-set (tree->list s1) (tree->list s2))))


;; 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))
