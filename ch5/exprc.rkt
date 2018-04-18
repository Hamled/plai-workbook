#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)])

(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (case (length sl)
                [(2) (uminusS (parse (second sl)))]
                [(3) (bminusS (parse (second sl)) (parse (third sl)))])]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(define (evaluate [e : ExprC]) : number
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (evaluate l) (evaluate r))]
    [multC (l r) (* (evaluate l) (evaluate r))]))
