;#lang racket

(define (checkkeyword x y)
  (or
    (xor (equal? y 'quote) (equal? x 'quote))
    (xor (equal? y 'lambda) (equal? x 'lambda))
    (xor (equal? y 'let) (equal? x 'let))
    (xor (equal? y 'if) (equal? x 'if))))

(define (makevar x y)
  (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

(define (handlelambda x y)
  (cond [(or (equal? '() y) (equal? '() x)) (values '() '())]
        [(equal? (length y) (length x))
        (cond [(not (equal? (car y) (car x)))
         (let-values (((newx newy) (handlelambda (cdr x) (cdr y))))
                (values (cons (car x) (cons (makevar (car x) (car y)) newx))
                        (cons (car y) (cons (makevar (car x) (car y)) newy))))]
         [else (handlelambda (cdr x) (cdr y))]
)]
   [else (values '() '())]))

(define (helpdet? x ax)
  (cond [(not (equal? '() ax)) (cond [(not (equal? (car ax) x))
                                (helpdet? x (cdr ax))][else ax])]
        [else #f]))
(define (helpget x ax)
  (cond [(not (equal? #f (helpdet? x ax))) (car (cdr (helpdet? x ax)))]
        [else x]))

(define (handlelista x newx)
  (cond [(equal? '() x) '()]
        [else (let ((anox (helpget (car (car x)) newx)))
                  (cond [(not(equal? (car (car x)) anox))
                         (foldr cons (list (cons anox (cdr (car x))))
                                     (handlelista (cdr x) newx))]
                        [else (foldr cons (list (car x))(handlelista (cdr x) newx))]))]))

(define (handlelistv x newx)
  (cond [(equal? '() x) '()]
        [else (let ((anox (helpget (car (cdr (car x))) newx)))
                  (cond [(not (equal? (car (cdr (car x))) anox))
                         (foldr cons (list (list (car (car x)) anox))
                                     (handlelistv (cdr x) newx))]
                       [else (foldr cons (list (car x)) (handlelistv (cdr x) newx))]))]))

(define (handleliste x newx)
  (cond [(equal? '() x) '()]
        [(list? x) (cond [(equal? 'let (car x)) (cons 'let (foldr cons
                          (list (handlelistv (car (cdr x)) newx))
                          (handleliste (cdr (cdr x)) '())))]
                         [(equal? 'lambda (car x)) (cons 'lambda
                          (handleliste (cdr x) newx))]
                   [else (cons (handleliste (car x) newx)
                         (handleliste (cdr x) newx))])]
    [else (helpget x newx)]))

(define (cmplambda x y)
  (let-values (((newx newy)(handlelambda (car (cdr x))(car (cdr y)))))
    (cond [(not (and (equal? '() newy) (equal? '() newx)))
           (let ((xxe (handleliste (car (cdr (cdr x))) newx))
                  (yye (handleliste (car (cdr (cdr y))) newy)))
            (let ((xxa (handleliste (car (cdr x)) newx))
                 (yya (handleliste (car (cdr y)) newy)))
             (cons 'lambda (expr-compare (list xxa xxe) (list yya yye)))))]
           [else (cons 'lambda (expr-compare (cdr x) (cdr y)))])))

(define (handlelist x y)
  (cond [(or (equal? '() y) (equal? '() x)) (values '() '())]
        [(equal? (length y) (length x))
        (cond [(not (equal? (car (car y)) (car (car x)))) 
               (let-values (((newx newy) (handlelist (cdr x) (cdr y))))
                (values (cons (car (car x)) (cons (makevar (car (car x)) (car (car y))) newx))
                        (cons (car (car y)) (cons (makevar (car (car x)) (car (car y))) newy))))] [else (handlelist (cdr x) (cdr y))])]
   [else (values '() '())]))

(define (cmplet x y)
  (let-values (((newx newy)(handlelist (car (cdr x))(car (cdr y)))))
    (cond [(not (and (equal? '() newy) (equal? '() newx)))
            (let ((xxe (handleliste (car (cdr (cdr x))) newx))
                  (yye (handleliste (car (cdr (cdr y))) newy)))
             (let ((xxa (handlelista (car (cdr x)) newx))
                 (yya (handlelista (car (cdr y)) newy)))
             (cons 'let (expr-compare (list xxa xxe) (list yya yye)))))]
           [else (cons 'let (expr-compare (cdr x)(cdr y)))])))

(define (deleteq x y)
  (quasiquote (if % (unquote x) (unquote y))))

(define (expr-compare x y)
  (cond [(equal? y x) x]
        [(and (equal? y #t) (equal? x #f)) (quote (not %))]
        [(and (equal? y #f) (equal? x #t)) (quote %)]
        [(and (list? y) (list? x)) (cond [(equal? (length y) (length x))
                                     (cond [(checkkeyword (car x) (car y))
                                     (deleteq x y)]
                          [(and (equal? (car y) 'quote) (equal? (car x) 'quote))
                           (cond [(not (equal? (cdr y) (cdr x)))(deleteq x y)]
                                 [else (quote x)])]
                        [(and (equal? (car y) 'lambda) (equal? (car x) 'lambda))
                         (cmplambda x y)]
                        [(and (equal? (car y) 'let) (equal? (car x) 'let))
                         (cmplet x y)]
  [else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))])]
        [else (deleteq x y)])]
  [else (deleteq x y)]))

(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y))))
       (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))))

(define test-expr-x '(if #f (lambda (a b) (or a b)) (let ((m 1) (n 2)) (list m n))))
(define test-expr-y '(if #t (lambda (a b) (and a b)) (let ((m 1) (n 2)) (cons m n))))
 
;(expr-compare 12 12)
;(expr-compare 12 20)
;(expr-compare #t #t)
;(expr-compare #f #f)
;(expr-compare #t #f)
;(expr-compare #f #t)
;(expr-compare 'a '(cons a b))
;(expr-compare '(cons a b) '(cons a b))
;(expr-compare '(cons a b) '(cons a c))
;(expr-compare '(cons (cons a b) (cons b c))
;              '(cons (cons a c) (cons a c)))
;(expr-compare '(cons a b) '(list a b))
;(expr-compare '(list) '(list a))
;(expr-compare ''(a b) ''(a c))
;(expr-compare '(quote (a b)) '(quote (a c)))
;(expr-compare '(quoth (a b)) '(quoth (a c)))
;(expr-compare '(if x y z) '(if x z z))
;(expr-compare '(if x y z) '(g x y z))
;(expr-compare '(let ((a 1)) (f a)) '(let ((a 2)) (g a)))
;(expr-compare '(let ((a c)) a) '(let ((b d)) b))
;(expr-compare ''(let ((a c)) a) ''(let ((b d)) b))
;(expr-compare '(+ #f (let ((a 1) (b 2)) (f a b)))
;              '(+ #t (let ((a 1) (c 2)) (f a c))))
;(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
;(expr-compare '((lambda (a b) (f a b)) 1 2)
;              '((lambda (a b) (f b a)) 1 2))
;(expr-compare '((lambda (a b) (f a b)) 1 2)
;              '((lambda (a c) (f c a)) 1 2))
;(expr-compare '(let ((a (lambda (b a) (b a))))
;                 (eq? a ((lambda (a b) (let ((a b) (b a)) (a b)))
;                         a (lambda (a) a))))
;              '(let ((a (lambda (a b) (a b))))
;                 (eqv? a ((lambda (b a) (let ((a b) (b a)) (a b)))
;                         a (lambda (b) a)))))               
