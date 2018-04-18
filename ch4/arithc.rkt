#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)])

(define (parse [s : s-expression]) : ArithS
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

(define (desugar [e : ArithS]) : ArithC
  (type-case ArithS e
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(define (evaluate [e : ArithC]) : number
  (type-case ArithC e
    [numC (n) n]
    [plusC (l r) (+ (evaluate l) (evaluate r))]
    [multC (l r) (* (evaluate l) (evaluate r))]))