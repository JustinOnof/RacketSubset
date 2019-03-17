(let ([z 0]))
(define (prim '()))
; build list every time it recurses which contains definition of lambda, addition, multiplication, etc.
(define (eval_first l)
(if(and (list? l)  (pair? (car l)))
(cond
  [(equal? (car l) '+ )] (addition(cdr l))
  [(equal? (car l) '- )] (subtraction(cdr l))
  [(equal? (car l) '* )] (multiplication(cdr l))
  [(equal? (car l) '/ )] (division(cdr l))
  [(equal? (car l) '( )] (eval_first (cdr l)))
(error "Something bad has happened in Eval_first function."))))

(define (eval_add x)
  (if (pair? x)
    (+ (car x) (cdr x))
    (error "Error in function addition, x is not a pair.")))

(define (eval_sub x)
  (if (pair? x)
    (- (car x) (cdr x))
    (error "Error in function subtraction, x is not a pair.")))

(define (eval_mult x)
  (if (pair? x)
    (* (car x) (cdr x))
    (error "Error in function multiplication, x is not a pair.")))

(define (eval_div x)
  (if (pair? x)
    (\ (car x) (cdr x))
    (error "Error in function division, x is not a pair.")))
