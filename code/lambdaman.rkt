; icfpc2014: icfp contest 2014
; Copyright (C) 2014  aankor (The sound of lambda team)
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket

(define (gather-labels code start) ; returns list of adresses
  (if (null? code)
      '()
      (if (eq? (caar code) 'label)
          (cons start (gather-labels (cdr code) start)); label is not instruction
          (gather-labels (cdr code) (+ start 1)))))

(define (sub-labels code labels)
  (define (sub-instr instr)
    (map (lambda (arg) (if (and (list? arg)(eq? (first arg) 'label))
                           (list-ref labels (second arg))
                           arg)) instr))
  (if (null? code)
      '()
      (if (eq? (caar code) 'label)
          (sub-labels (cdr code) labels)
          (cons (cons (caar code) (sub-instr (cdar code))) (sub-labels (cdr code) labels)))))
   
(define (asm->output code start)
  (define (translate sym)
    (cond [(string? sym) (string-append ";" sym)]
          [(integer? sym) (number->string sym)]
          [(symbol? sym) (symbol->string sym)]))
  (let* ([labels (gather-labels code start)]
         [outcode (sub-labels code labels)])
    (string-join (map (lambda (instr)
                        (string-join (map translate instr) " ")) outcode) "\n" )))

(define additional-functions (make-parameter (mcons '() '())))
(define last-lambda-index (make-parameter -1))
(define named-functions (make-parameter (make-hash)))

(define (add-addition-function body env retcode is-lambda [l (additional-functions)] [i 0])
  (if (null? (mcdr l))
      (begin (set-mcdr! l (mcons '() '()))
             (if is-lambda (last-lambda-index i) '())
             (set-mcar! l (append (expr->asm body env) `((,retcode))))
             i)
      (add-addition-function body env retcode is-lambda (mcdr l) (+ i 1))))


(define (expr->asm expr env)
  (define (parse-variable var env n i)
    (if (null? env)
        (error "variable not found " var)
        (if (null? (car env))
            (parse-variable var (cdr env) (+ n 1) 0)
            (if (eq? var (caar env))
                `(LD  ,n ,i)
                (parse-variable var (cons (cdar env) (cdr env)) n (+ i 1))))))
  
  (define (parse-function-name expr)
    (let ([index (hash-ref (named-functions) expr #f)])
      (if index `(LDF (label ,index)) #f)))
  
  (define (parse-func-call fun args env)
    `(,@(append* (map (lambda (e) (expr->asm e env)) args))
      ,@(expr->asm fun env)
      (AP ,(length args))))
  
  (define (parse-recur args env)
    `(,@(append* (map (lambda (e) (expr->asm e env)) args))
      (LDF (label ,(last-lambda-index)))
      (AP ,(length args))))
  
  (define (parse-lambda args body env)
    `(LDF (label ,(add-addition-function body (cons args env) 'RTN #t))))

  (define (parse-library funcs body env)
    (if (null? funcs)
        (expr->asm body env)
        (begin (hash-set! (named-functions)
                          (caaar funcs)
                          (add-addition-function (cadar funcs) (list (cdaar funcs)) 'RTN #t))
               (parse-library (cdr funcs) body env))))
  
  (define (parse-if test then else env)
    (let ([then-index (add-addition-function then env 'JOIN #f)]
          [else-index (add-addition-function else env 'JOIN #f)])
      `(,@(expr->asm test env)
        (SEL (label ,then-index) (label ,else-index)))))
  
  (define (parse-list l env)
    (if (null? l)
        '((LDC 0))
        `(,@(expr->asm (car l) env) ,@(parse-list (cdr l) env) (CONS))))
    
  (define (parse-fold operator operands env)
    (if (null? (cdr operands))
        (expr->asm (car operands) env)
        (append (expr->asm (car operands) env) (parse-fold operator (cdr operands) env) `((,operator)))))
  
  (define (parse-let vars body env)
    `(,@(append* (map (lambda (e) (expr->asm (cadr e) env)) vars))
      (LDF (label ,(add-addition-function body (cons (map car vars) env) 'RTN #f)))
      (AP ,(length vars))))
  
  (cond [(integer? expr) `((LDC ,expr))]
        [(symbol? expr) (list (or (parse-function-name expr) (parse-variable expr env 0 0)))]
        [(list? expr) (case (car expr)
                        ['+ (parse-fold 'ADD (cdr expr) env)]
                        ['- `(,@(expr->asm (cadr expr) env) ,@(expr->asm (caddr expr) env) (SUB))]
                        ['* (parse-fold 'MUL (cdr expr) env)]
                        ['/ `(,@(expr->asm (cadr expr) env) ,@(expr->asm (caddr expr) env) (DIV))]
                        ['= `(,@(expr->asm (cadr expr) env) ,@(expr->asm (caddr expr) env) (CEQ))]
                        ['> `(,@(expr->asm (cadr expr) env) ,@(expr->asm (caddr expr) env) (CGT))]
                        ['>= `(,@(expr->asm (cadr expr) env) ,@(expr->asm (caddr expr) env) (CGTE))]
                        ['< `(,@(expr->asm (caddr expr) env) ,@(expr->asm (cadr expr) env) (CGT))]
                        ['<= `(,@(expr->asm (caddr expr) env) ,@(expr->asm (cadr expr) env) (CGTE))]
                        ['atom? `(,@(expr->asm (cadr expr) env) (ATOM))]
                        ['lambda (list (parse-lambda (second expr) (third expr) env))]
                        ['recur  (parse-recur (cdr expr) env)]
                        ['if (parse-if (second expr) (third expr) (fourth expr) env)]
                        ['cons `(,@(expr->asm (cadr expr) env) ,@(expr->asm (caddr expr) env) (CONS))]
                        ['car `(,@(expr->asm (cadr expr) env) (CAR))]
                        ['cdr `(,@(expr->asm (cadr expr) env) (CDR))]
                        ['list (parse-list (cdr expr) env)]
                        ['library (parse-library (second expr) (third expr) env)]
                        ['let (parse-let (second expr) (third expr) env)]
                        ['or (expr->asm (let loop ([exprs (cdr expr)])
                                          (if (null? (cdr exprs))
                                              (car exprs)
                                              `(if ,(car exprs) 1 ,(loop (cdr exprs))))) env)]
                        ['and (expr->asm (let loop ([exprs (cdr expr)])
                                            (if (null? (cdr exprs))
                                                (car exprs)
                                                `(if ,(car exprs) ,(loop (cdr exprs)) 0))) env)]
                        ['dbug `(,@(expr->asm (cadr expr) env) (DBUG) ,@(expr->asm (caddr expr) env))]
                        [else (parse-func-call (car expr) (cdr expr) env)])]))

(define (expr->output expr env start)
  (asm->output (expr->asm expr env) start))

(define (mainexpr->asm expr)
  (define (add-labels funcs i)
    (if (null? (mcdr funcs)) '()
        (cons (list 'label i) (append (mcar funcs)  (add-labels (mcdr funcs) (+ i 1))))))
  (parameterize ([additional-functions (mcons '() '())]
                 [last-lambda-index -1]
                 [named-functions (make-hash)])
    (append (expr->asm expr '((world-state undocumented))) '((RTN))
            (add-labels (additional-functions) 0))))
(define (mainexpr->output expr)
  (asm->output (mainexpr->asm expr) 0))

(define stdlib '(((print x) (dbug x x))
                 ((id x) x)
                 ((elt l i)(if i (recur (cdr l) (- i 1)) (car l)))
                 ((elt2 m p)(elt (elt m (cdr p)) (car p)))
                 ((set l i v) (if i (cons (car l) (recur (cdr l) (- i 1) v)) (cons v (cdr l))))
                 ((mod x y) (- x (* (/ x y) y)))
                 ((not x) (if x 0 1))
                 ((abs x) (if (< x 0) (- 0 x) x))
                 ((get-ghosts world) (car (cdr (cdr world))))
                 ((is-pill x) (+ (= x 2) (= x 3)))

                 ((qget p s)(if (atom? s)
                                0
                                (if (and (= (car p) (car (car (car s))))
                                         (= (cdr p) (cdr (car (car s)))))
                                    (cdr (car s))
                                    (recur p (cdr s)))))
                 
                 ((eat-ghost x y ghosts)
                  (if (atom? ghosts)
                      0
                      (let ((p (car (cdr (car ghosts))))
                            (v (car (car ghosts))))
                        (if (and (and (= (car p) x) (= (cdr p) y)) (= v 1))
                            1
                            (recur x y (cdr ghosts))))))
                 ((is-ghost-near x y vit ghosts)
                  (if (or (atom? ghosts) (> vit 137))
                      0
                      (let ((p (car (cdr (car ghosts))))
                            (v (car (car ghosts))))
                        (if (and (or (= v 0) (<= vit 137)) (<= (+ (abs (- (car p) x)) (abs (- (cdr p) y))) 1))
                            1
                            (recur x y vit (cdr ghosts))))))
                 
                 ((is-opposite x y)
                  (or (= (- x y) 2)
                      (= (- y x) 2)))

                 ((add-point0 points p d vit world)
                  (let ((cell (elt2 (car world) p)))
                    (if cell
                        (if (is-ghost-near (car p) (cdr p) vit (get-ghosts world))
                            (cons (cons p (cons cell (cons 10 (cons d vit)))) points) ; scared point
                            (cons (cons p (cons cell (cons d (cons d vit)))) points))
                        points)))
                 
                 ((add-point points p d origd vit oldd seen1 seen2 world)
                  (let ((cell (elt2 (car world) p)))
                    (if (and cell
                             (not (is-opposite oldd d))
                             (atom? (qget p points))
                             (atom? (qget p seen1))
                             (atom? (qget p seen2)))
                        (if (is-ghost-near (car p) (cdr p) vit (get-ghosts world))
                            (cons (cons p (cons cell (cons 10 (cons origd vit)))) points) ; scared point
                            (cons (cons p (cons cell (cons d (cons origd vit)))) points))
                        points)))
                 
                 ((update-vit vit cell)
                  (if (= cell 2)
                      (- vit 137)
                      (if (= cell 3)
                          2540
                          (- vit 127))))
                      
                 ((get-next-points next current fullcurrent old-current world)
                  (if (atom? current)
                      next
                      (let ((cur (car current)))
                        (let ((p (car cur))
                              (x (car (car cur)))
                              (y (cdr (car cur)))
                              (oldd (car (cdr (cdr cur))))
                              (origd (car (cdr (cdr (cdr cur)))))
                              (vit (update-vit (cdr (cdr (cdr (cdr cur)))) (car (cdr cur)))))
                          (if (= oldd 10) ; scared point: copy it
                              (recur (cons cur next) (cdr current) fullcurrent old-current world)
                          (recur
                           (add-point
                            (add-point
                             (add-point
                              (add-point next (cons x (- y 1)) 0 origd vit oldd fullcurrent old-current world)
                              (cons (+ x 1) y) 1 origd vit oldd fullcurrent old-current world)
                             (cons x (+ y 1)) 2 origd vit oldd fullcurrent old-current world)
                            (cons (- x 1) y) 3 origd vit oldd fullcurrent old-current world)
                           (cdr current) fullcurrent old-current world))))))
                 
                 ((is-fruit world cell)
                  (and (= cell 4) ; fruit is here
                       (cdr (cdr (cdr world))))) ; and its spawned
                 
                 ((update-state next state have-moves world)
                  (if (atom? next)
                      (if have-moves state (cons state 100))
                      (let ((p (car (car next)))
                            (cell (car (cdr (car next))))
                            (curd (car (cdr (cdr (car next)))))
                            (d (car (cdr (cdr (cdr (car next))))))
                            (vit (cdr (cdr (cdr (cdr (car next))))))
                            (sd (car state))
                            (sg (car (cdr state)))
                            (sf (car (cdr (cdr state))))
                            (sp (car (cdr (cdr (cdr state))))))
                        (if (= curd 10) ; ignore scared
                            (recur (cdr next) (if (= sd 10) (cons d (cdr state)) state) have-moves world)
                        (if (and (= sg 10) (> vit 0) (eat-ghost (car p) (cdr p) (get-ghosts world)))
                            (recur (cdr next) (list sd d sf sp) 1 world)
                            (if (and (= sf 10) (is-fruit world cell))
                                (recur (cdr next) (list sd sg d sp) 1 world)
                                (if (and (= sp 10) (is-pill cell))
                                    (recur (cdr next) (list sd sg sf d) 1 world)
                                    (recur (cdr next) state 1 world))))))))
                 ((decide state)
                  (if (atom? state)
                      10
                      (if (= (car state) 10)
                          (recur (cdr state))
                          (car state))))
                        
                 ((search active old-active state world counter)
                  (let ((new-state (update-state active state 0 world)))
                    (if (atom? (cdr new-state))
                        (car new-state)
                        (if (or (not (= (car (cdr new-state)) 10)) (< counter 0))
                            new-state
                        (recur (get-next-points 0 active active old-active world) active new-state world (- counter 1))))))
                 
                 ((choose-not points dir)
                  (if (atom? points)
                      10
                      (let ((d (car (cdr (cdr (cdr (car points)))))))
                      (if (= d dir)
                          (recur (cdr points) dir)
                          d))))
                          
                 
                 ((find-next-move world)
                  (let ((p (car (cdr (car (cdr world)))))
                        (vit (car (car (cdr world)))))
                    (let ((x (car p))
                          (y (cdr p)))
                      (let ((start-points (add-point0
                               (add-point0
                                (add-point0
                                 (add-point0 0 (cons x (- y 1)) 0 vit world)
                                 (cons (+ x 1) y) 1 vit world)
                                (cons x (+ y 1)) 2 vit world)
                               (cons (- x 1) y) 3 vit world)))
                        (let ((res-state (search start-points 0 (list 10 10 10 10) world 100)))
                          (let ((sol (decide (cdr res-state))))
                            (if (= sol 10)
                                (let ((sol2 (choose-not start-points (car res-state))))
                                  (if (= 10 sol2) (car (cdr (cdr (cdr (car start-points))))) sol2))
                                sol)))))))))
(define step1-prog
  `(library ,stdlib
            (cons 0 (lambda (state world-state)
                      (let ((next (find-next-move world-state))) (cons next next))))))

(printf (mainexpr->output step1-prog))

;(define minlib '(
;  ((oldor x y)(if x 1 y))
;))
;
;(define _main
;  `(library ,minlib
;            (cons 0 (lambda (aistate world-state)
;                       (or aistate (or (car (car world-state)) (cdr (cdr world-state)) ) )
;            ))
;   ))
;
;(printf (mainexpr->output _main))
