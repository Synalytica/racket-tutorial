#lang racket

(require eopl)

(define-datatype ast ast?
  [num (datum number?)]
  [bool (datum boolean?)]
  [ifte (test ast?) (then ast?) (else-ast ast?)]
  [function
   (formals (list-of id?))
   (body ast?)]
  [app (rator ast?) (rands (list-of ast?))]
  [id-ref (sym id?)]
  [assume (binds  (list-of bind?)) (body ast?)]
  [applyf (fid ast?) (rands (list-of ast?))]
)

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))
(define env? procedure?)


;;; lookup-env: [env?  symbol?] -> any/c
;;; lookup-env: throws "unbound identifier" error
(define lookup-env
  (lambda (e x)
    (e x)))

;;; empty-env : () -> env?
(define empty-env
  (lambda ()
    (lambda (x)
      (error 'empty-env "unbound identifier ~a" x))))

;;; extended-env :
;;;    [(list-of symbol?) (list-of any/c) env?] -> env?
(define extended-env
  (lambda (syms vals outer-env)
    (lambda (x)
      (let ([j (list-index syms x)])
        (cond
          [(= j -1) (lookup-env outer-env x)]
          [#t (list-ref vals j)])))))

;;; Returns the loction of the element in a list, -1 if the
;;; element is absent.

;;; list-index : [(listof any/c)  any/c] -> 
(define list-index
  (lambda (ls a)
    (letrec ([loop
               (lambda (ls ans)
                 (cond
                   [(null? ls) -1]
                   [(eq? (first ls) a) ans]
                   [#t (loop (rest ls) (+ 1 ans))]))])
      (loop ls 0))))
(define *keywords*
  '(ifte function assume))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))

;;; parse :: any/c -> ast?  Raises exception exn?
;;; Fill in the function parse here
(define parse-assumptions
    (λ (exp)
      (if (null? exp)
          '()
          (cons (make-bind (first (first exp)) (parse (second (first exp)))) (parse-assumptions (cdr exp)))
          )
      )
    )

(define (parse exp)
    (cond
      [(number? exp) (num exp)]  ;; simple number case
      [(boolean? exp) (bool exp)]  ;; boolean case
      [(id? exp) (id-ref exp)]  ;; id reference
      [(and (list? exp) (= (length exp) 4)  ;; if/then/else case
            (eq? (first exp) 'if)
            (boolean? (second exp)))
       (ifte (parse (second exp)) (parse (third exp)) (parse (fourth exp)))]
      [(and (list? exp) (= (length exp) 3)) ;; binary ops
       (cond
         [(eq? (first exp) 'assume) (assume (parse-assumptions (second exp)) (parse (third exp)))]
         [(eq? (first exp) 'function) (function (second exp) (parse (third exp)))]
         [(and (list? exp) (pair? (first exp)) (eq? (first (first exp)) 'function)) (applyf (parse (first exp)) (map parse (rest exp)))]
         [(id? (first exp)) (app (parse (first exp)) (map parse (rest exp)))]
         [else (raise-parse-error "invalid syntax ~a") exp]  ;; invalid operation
         )]
      [else (raise-parse-error "invalid syntax ~a") exp]  ;; invalid operation
      )
    )
(define-datatype proc proc?
  [prim-proc
    ;; prim refers to a scheme procedure
    (prim procedure?)
    ;; sig is the signature
    (sig (list-of procedure?))] 
  [closure
    (formals (list-of symbol?))
    (body ast?)
    (env env?)])

;;; prim? : proc? -> boolean?
(define prim-proc?
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #t]
      [else #f])))

(define closure? 
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #f]
      [else #t])))
;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (or/c number? boolean? proc?))
;;; denotable-value? :any/c -> boolean?
(define denotable-value?
  (or/c number? boolean? proc?))
;;; implement all procedures in the list
(define (+p lhs rhs) (+ lhs rhs))
(define (-p lhs rhs) (- lhs rhs))
(define (*p lhs rhs) (* lhs rhs))
(define (/p lhs rhs) (/ lhs rhs))
(define (<p lhs rhs) (< lhs rhs))
(define (<=p lhs rhs) (<= lhs rhs))
(define (eq?p lhs rhs) (equal? lhs rhs))
(define (0?p operand) (equal? operand 0))
(define (!p operand) (not operand))
(define *init-env*
  (extended-env
   '(+ - * / < <= eq? 0? !)
   (list +p -p *p /p <p <=p eq?p 0?p !p)
   (empty-env)))
(define apply-closure
    (λ (closure-exp rands outer-env)
      (cases proc closure-exp
        [closure (formals body environment)
                 (let ((arguments (map (lambda (exp)(eval-ast exp outer-env)) rands)))
                (eval-ast body  (extended-env  formals arguments environment)))]
        [prim-proc (prim sig) (error "not a closure")]
        )
      )
    )
(define eval-ast
    (λ (tree environment)
      (cases ast tree
        [num (n) n]
        [bool (b) b]
        [id-ref (exp) (let ((mock-env environment)) (lookup-env mock-env exp))]
        [ifte (condition if-true-exp if-false-exp)
              (cond
                [(typecheck-bool (eval-ast condition environment)) (eval-ast if-true-exp environment)]
                [(not (typecheck-bool (eval-ast condition environment))) (eval-ast if-false-exp environment)]
                )]
        [assume (bindings body)
                (let ((tuples (map (λ (var) (list (bind-id var)
                                                  (eval-ast (bind-ast var) environment)))
                                   bindings)))
                  (eval-ast body (extended-env (list (first (first tuples)))
                                               (list (second (first tuples)))
                                               environment))
                  )]
        [function (params body) (closure params body environment)]
        [applyf (func-id params) (letrec ((closures (eval-ast func-id environment))) (apply-closure closures params environment))]
        [app (rator rands) (letrec ((applc (eval-ast rator environment))
                                    (arguments (map (λ (arg) (eval-ast arg environment)) rands)))
                                    (apply applc arguments))]
        )
      )
    )


(provide (all-defined-out))
