#lang racket
(require 2htdp/image)
(define (how-many list)
  (cond
    [(empty? (cdr list)) 1]
    [else (+ 1 (how-many (cdr list)))]))
 
;(how-many '(1 2 34 5))

(define (sum list)
  (cond
    [(empty? (cdr list)) (car list)]
    [else (+ (car list) (sum (cdr list)))]))

;(sum '(5 20 -10))

(define (check-all-true list)
  (cond
    [(empty? list) (error "empty list~~")]
    [else (all-true list)]))
(define (all-true l)
  (cond
    [(empty? (cdr l)) (car l)]
    [else (and (car l) (all-true (cdr l)))]))
;(check-all-true '(#t #t #t))

(define (check-one-true list)
  (cond
    [(empty? list) (error "empty list~~")]
    [else (one-true list)]))
(define (one-true l)
  (cond
    [(empty? (cdr l)) (car l)]
    [else (or (car l) (one-true (cdr l)))]))
;(check-one-true '(#f #f #f))

(define (cat l)
  (cond
    [(empty? l) ""]
    [(string? (car l)) (string-append (car l) (cat (cdr l)))]
    [else "#false"]))
;(cat (cons "ab" (cons "cd" (cons "ef" '()))))

(define (ill-sized? list-image n)
  (cond
    [(empty? list-image) #false]
    [(= n (image-width (car list-image)) (image-height (car list-image))) (car list-image)]
    [else (ill-sized? (cdr list-image) n)]))
#|(ill-sized? 
 (cons (rectangle 20 20 "solid" "silver")
       (cons (rectangle 30 30 "solid" "seagreen")
             (cons (rectangle 40 40 "solid" "silver")
                   (cons (rectangle 50 50 "solid" "seagreen") '()))))
 10)|#

(define (average alot)
  (/ (sum alot) (how-many alot)))
;(average (cons 1 (cons 2 (cons 3 '()))))
(define (check-average alot)
  (cond
    [(empty? alot) (error "empty list~~")]
    [else (average alot)]))
;(sum (cons 1 (cons 2 (cons 3 '()))))

(define (check-sort alot)
  (cond
    [(empty? alot) (error "empty list~~")]
    [else (sorted>? alot)]))
(define (sorted>? alot)
  (cond
    [(empty? (cdr alot)) #t]
    [else (and (> (car alot) (car (cdr alot)))
               (sorted>? (cdr alot)))]))
;(check-sort '(120 52 34 5))
(define-struct loc [x y])
;(make-list 2 (make-loc 1 2))

(define (check-copier n str)
  (cond
    [(negative? n) (error "n is not negative~~~")]
    [else (copier n str)]))
(define (copier n str)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons str (copier (sub1 n) str))]
    [else '(-1)]))
;(check-copier 0.9 "str")

(define (check-add n m)
  (cond
    [(zero? n) m]
    [(negative? n) (sub n m)]
    [else (add n m)]))

(define (add n m)
  (cond
    [(zero? n) m]
    [else (add1 (add (sub1 n) m))]))
(define (sub n m)
  (cond
    [(zero? n) m]
    [else (sub1 (sub (add1 n) m))]))

;(check-add -10 -10)

(define (check-muliply n m)
  (cond
    [(or (= n 0) (= m 0)) 0]
    [(and (positive? n) (positive? m)) (multiply n m)]
    [(and (negative? n) (negative? m)) (multiply (- n) (- m))]
    [(negative? n) (multiply (- n) (- m))]
    [(negative? m) (multiply n m)]))

(define (multiply n m)
  (cond
    [(= n 0) 0]
    [else (+ m (multiply (sub1 n) m))]))
; (check-muliply 3 10)
; (check-muliply -3 -10)
; (check-muliply -3 10)
; (check-muliply 3 -10)
; (check-muliply 0 -10)
; (check-muliply 3 0)

(define (col n img)
  (cond
    [(zero? (sub1 n)) img]
    [else (above img (col (sub1 n) img))]))
(col 22 (rectangle 10 10 "outline" "black"))

(define (row n img)
  (cond
    [(zero? (sub1 n)) img]
    [else (beside img (row (sub1 n) img))]))
(row 22 (rectangle 10 10 "outline" "black"))

(define (hall m n img)
  (cond
    [(zero? (sub1 m)) (row n img)]
    [else (above (hall (sub1 m) n img) (row n img))]))
;(hall 20 10 (rectangle 10 10 "outline" "black"))
(define background (overlay
                    (hall 20 10 (rectangle 10 10 "outline" "black"))
                    (empty-scene 100 200)))
(define-struct LOC [x y])
(define red-dot (circle 4 "solid" "red"))
(define (add-balloons list)
  (cond
    [(empty? list) background]
    [else (place-image red-dot (LOC-x (car list)) (LOC-y (car list)) (add-balloons (cdr list)))]))
#|(add-balloons
 (cons (make-LOC 10 20)
       (cons (make-LOC 20 40)
             (cons (make-LOC 30 60)
                   (cons (make-LOC 40 80)
                         (cons (make-LOC 50 100)
                               (cons (make-LOC 60 120)
                                     (cons (make-LOC 70 140)
                                           (cons (make-LOC 80 160)
                                                 (cons (make-LOC 90 180)
                                                     (cons (make-LOC 100 200)'())))))))))))
|#
