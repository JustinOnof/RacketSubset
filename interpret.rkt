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

(define (eval_add x state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (+ (car x) (execute(car(cdr x) state)))
      (+ (car x) (car(cdr x)))
    (error "Error in function addition, x is not a pair.")))

(define (eval_sub x state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (- (car x) (execute(car(cdr x) state)))
      (- (car x) (car(cdr x)))
    (error "Error in function subtraction, x is not a pair.")))

(define (eval_mult x state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (* (car x) (execute(car(cdr x) state)))
      (* (car x) (car(cdr x)))
    (error "Error in function multiplication, x is not a pair.")))

(define (eval_div x state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (\ (car x) (execute(car(cdr x) state)))
      (\ (car x) (car(cdr x)))
    (error "Error in function division, x is not a pair.")))


(define (eval_equal x state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (equal? (car x) (execute(car(cdr x) state)))
      (equal? (car x) (car(cdr x)))
    (error "Error in function division, x is not a pair.")))

(define (eval_eq x state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (= (car x) (execute(car(cdr x) state)))
      (= (car x) (car(cdr x)))
    (error "Error in function division, x is not a pair.")))

;staying the same or increasing
(define (eval_LTE x state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (<= (car x) (execute(car(cdr x) state)))
      (<= (car x) (car(cdr x)))
    (error "Error in function division, x is not a pair.")))

;increasing order
(define (eval_LT x state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (< (car x) (execute(car(cdr x) state)))
      (< (car x) (car(cdr x)))
    (error "Error in function division, x is not a pair.")))

;decreasing order or staying the same
(define (eval_GTE x  state)
(if (pair? x)
  (if(pair? (car(cdr x))))
    (>= (car x) (execute(car(cdr x) state)))
    (>= (car x) (car(cdr x)))
  (error "Error in function division, x is not a pair.")))

;decreasing order
(define (eval_GT x  state)
  (if (pair? x)
    (if(pair? (car(cdr x))))
      (> (car x) (execute(car(cdr x) state)))
      (> (car x) (car(cdr x)))
    (error "Error in function division, x is not a pair.")))
