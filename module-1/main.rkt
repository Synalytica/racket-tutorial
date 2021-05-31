#lang racket

(require eopl)
(require rackunit)
(require rackunit/text-ui)
(require "datatypes.rkt")

(define repeat
   (λ (n obj)
     (if (= n 0)
         '()
         (cons obj (repeat (- n 1) obj))
         )
     )
   )
(define invert
  (λ (2-list)
    (if (null? 2-list)
        '()
        (cons (list (car (cdr (car 2-list))) (car (car 2-list))) (invert (cdr 2-list)))
        )
    )
  )
(define count-occurrences
    (λ (el lst)
      (if (null? lst)
          0
          (if (equal? el (car lst))
              (+ 1 (count-occurrences el (cdr lst)))
              (+ 0 (count-occurrences el (cdr lst)))
              )
          )
      )
    )

(define single-pairing
    (λ (el lst)
    (if (null? lst)
        '()
        (cons (list el (car lst)) (single-pairing el (cdr lst)))
      )
    )
  )

(define product-helper
    (λ (lstl lstr)
      (if (or (null? lstl) (null? lstr))
          '()
          (append (single-pairing (car lstl) lstr) (product-helper (cdr lstl) lstr))
          )
      )
    )

(define product
    (λ (lstl lstr)
      (if (or (null? lstl) (null? lstr))
          (if (null? lstl) lstr lstl)
          (product-helper lstl lstr)
          )
      )
    )
(define every
    (λ (pred lst)
      (if (null? lst)
          #t
          (and (pred (car lst)) (every pred (cdr lst)))
          )
      )
    )
(define merge
    (λ (lstl lstr)
      (if (or (null? lstl) (null? lstr))
          (if (null? lstl) lstr lstl)
          (if (< (car lstl) (car lstr))
              (cons (car lstl) (merge (cdr lstl) lstr))
              (cons (car lstr) (merge lstl (cdr lstr)))
              )
          )
      )
    )
(define flatten
    (λ (obj)
      (if (null? obj)
          '()
          (if (list? obj)
              (append (flatten (car obj)) (flatten (cdr obj)))
              (list obj)
              )
          )
      )
    )

(define traverse/preorder
    (λ (tree)
      (if (integer? tree)
          (list tree)
      (cases full-binary-tree tree
        (leaf-node (n) (list n))
        (internal-node (value subtreel subtreer) (append (list value) (traverse/preorder subtreel) (traverse/preorder subtreer)))
        )
      )
    )
    )
(define traverse/inorder
    (λ (tree)
      (if (integer? tree)
          (list tree)
      (cases full-binary-tree tree
        (leaf-node (n) (list n))
        (internal-node (value subtreel subtreer) (append (traverse/inorder subtreel) (list value) (traverse/inorder subtreer)))
        )
      )
    )
    )
(define traverse/postorder
    (λ (tree)
      (if (integer? tree)
          (list tree)
      (cases full-binary-tree tree
        (leaf-node (n) (list n))
        (internal-node (value subtreel subtreer) (append (traverse/postorder subtreel) (traverse/postorder subtreer) (list value) ))
        )
      )
    )
    )

(define count-nodes
   (λ (tree)
     (cases full-binary-tree tree
       (leaf-node (n) 1)
       (internal-node (value subtreel subtreer) (+ 1 (count-nodes subtreel) (count-nodes subtreer)))
     )
   )
   )
(define count-leaves
    (λ (tree)
      (cases full-binary-tree tree
        (leaf-node (n) 1)
        (internal-node (value subtreel subtreer) (+ (count-leaves subtreel) (count-leaves subtreer)))
      )
    )
    )
(define count-internal
    (λ (tree)
      (cases full-binary-tree tree
        (leaf-node (n) 0)
        (internal-node (value subtreel subtreer) (+ 1 (count-internal subtreel) (count-internal subtreer)))
      )
    )
    )

(define tree/map
    (λ (fn tree)
      (cases full-binary-tree tree
        (leaf-node (n) (leaf-node (fn n)))
        (internal-node (value subtreel subtreer) (internal-node (fn value) (tree/map fn subtreel) (tree/map fn subtreer))))
      )
    )
(define value-at-path
    (λ (path tree)
      (if (null? path)
          (cases full-binary-tree tree
            (leaf-node (n) n)
            (internal-node (value subtreel subtreer) value)
            )
          (cases full-binary-tree tree
            (leaf-node (n) '())
            (internal-node (value subtreel subtreer)
                           (if (equal? (car path) "left")
                               (value-at-path (cdr path) subtreel)
                               (value-at-path (cdr path) subtreer))
                           )
            )
          )
      )
    )

(define traverse-path
    (λ (term tree cur-path)
      (cases full-binary-tree tree
        (leaf-node (node) (if (equal? node term) cur-path '()))
        (internal-node (value subtreel subtreer) (if (equal? value term)
                                                     cur-path
                                                     (append (traverse-path term subtreel (append cur-path (list "left")))
                                                             (traverse-path term subtreer (append cur-path (list "right")))
                                                             )
                                                     )
                       )
        )
      )
    )
(define search
    (λ (term tree)
      (traverse-path term tree '())
      )
    )
(define update
    (λ (path func tree)
      (cases full-binary-tree tree
        (leaf-node (value) (lnode (if (null? path) (func value) value)))
        (internal-node (value subtreel subtreer) (if (null? path)
                                                     (inode (func value) subtreel subtreer)
                                                     (if (equal? (car path) "left")
                                                         (inode value (update (cdr path) func subtreel) subtreer)
                                                         (inode value subtreel (update (cdr path) func subtreer))
                                                         )
                                                     )
                       )
        )
      )
    )
;;; 

;;; exporting only the required function
(provide repeat)
(provide invert)
(provide count-occurences)
(provide product)
(provide every)
(provide merge)
(provide flatten)

(provide traverse/preorder)
(provide traverse/inorder)
(provide traverse/postorder)

(provide count-nodes)
(provide count-leaves)
(provide count-internal)

(provide tree/map)
(provide value-at-path)

(provide search)
(provide update)
;;; (provide tree/insert)
