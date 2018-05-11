#lang racket
(require 2htdp/universe)
(require 2htdp/image)

; distances in terms of pixels
(define HEIGHT 800) 
(define WIDTH 1000)

; graphical constants
(define red-dot (circle 4 "solid" "red"))

;some struct
(define-struct LOC [x y])
(define-struct VEL [dx dy])
(define-struct ball [loc vel])
(define-struct work [num employee rate hours])
(define-struct paycheck [num employee pay])

(define (col n img)
  (cond
    [(zero? (sub1 n)) img]
    [else (above img (col (sub1 n) img))]))
;(col 22 (rectangle 10 10 "outline" "black"))

(define (row n img)
  (cond
    [(zero? (sub1 n)) img]
    [else (beside img (row (sub1 n) img))]))
;(row 22 (rectangle 10 10 "outline" "black"))
(define (hall m n img)
  (cond
    [(zero? (sub1 m)) (row n img)]
    [else (above (hall (sub1 m) n img) (row n img))]))
;(hall 20 10 (rectangle 10 10 "outline" "black"))
(define background (overlay
                    (hall 20 20 (rectangle 20 20 "outline" "black"))
                    (empty-scene 400 400)))

(define (tock list)
  (cond
    [(empty? list) list]
    [else (cons (make-ball
                 (make-LOC (+ (LOC-x (ball-loc (car list))) (VEL-dx (ball-vel (car list))))
                           (+ (LOC-y (ball-loc (car list))) (VEL-dy (ball-vel (car list)))))
                 (ball-vel (car list)))
                (tock (cdr list)))]))

(define (pkey list key)
  (cond
    [(key=? key " ") (cons (make-ball (make-LOC 0 0) (make-VEL 1 2)) list)]
    [else list]))

(define (render list)
  (cond
    [(empty? list) background]
    [else (place-image red-dot
                       (LOC-x (ball-loc (car list)))
                       (LOC-y (ball-loc (car list)))
                       (render (cdr list)))]))

(define (how-many list)
  (cond
    [(empty? list) 0]
    [else (+ 1 (how-many (cdr list)))]))

(define (game-over? list)
  (cond
    [(< 5 (how-many list)) #t]
    [else #f]))

(define (last-render list)
  (cond
    [(empty? list) (text "" 10 "red")]
    [else  (beside (text (string-append
                  "("
                  (number->string (LOC-x (ball-loc (car list))))
                  ","
                  (number->string (LOC-y (ball-loc (car list))))
                  ")") 10 "red")
          (last-render (cdr list)))]))

(define (throw-balls list)
  (big-bang list
    [on-tick tock]
    [on-key pkey]
    [to-draw render]
    [stop-when game-over? last-render]))

;(last-render (cons (make-ball (make-LOC 20 20) (make-VEL 1 1))
;                   (cons (make-ball (make-LOC 40 40) (make-VEL 1 1)) '())))
;(throw-balls '())


; List-of-string String -> N
; determines how often s occurs in los
(define (count los s)
  (cond
    [(empty? los) 0]
    [else (+ (if (string=? (car los) s) 1 0)
             (count (cdr los) s))]))
;(count '("1" "1" "2") "2")


; Number Son.L -> Son.L
; removes x from s
(define s1.L
  (cons 1 (cons 1 empty)))
(define (set-.L x s)
  (remove* x s))
;(set-.L '(1) s1.L)

	
; Number Son.R -> Son.R
; removes x from s
(define s1.R
  (cons 1 '()))
(define (set-.R x s)
  (remove x s))
;(remove 1 s1.R)

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (make-paycheck (work-num h)
                 (work-employee h)
                 (* (work-rate h) (work-hours h))))
;(wage* (cons 4 (cons 2 '())))

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) empty]
    [else (cons (wage (car whrs)) (wage* (cdr whrs)))]))

(wage* (cons (make-work 123 "Matthew" 12.95 45)
      (cons (make-work 456 "Robby" 11.95 39)
            '())))

(define (check-wage* whrs)
  (cond
    [(empty? whrs) #t]
    [else (and (> 100 (work-hours (car whrs)))
               (check-wage* (cdr whrs)))]))
;(check-wage* (cons (make-work "Matthew" 12.95 45)
;      (cons (make-work "Robby" 11.95 39)
;            '())))

;k = c + 273.15
(define (ctok list)
  (cond
    [(empty? list) empty]
    [else (cons (+ 273.15 (car list)) (ctok (cdr list)))]))
;(ctok '(100 200 0 -100 -200))

(define (substitute old new list)
  (cond
    [(empty? list) '()]
    [else (cons (if (string=? old (car list))
                    new
                    (car list))
                (substitute old new (cdr list)))]))
;(substitute "123" "456" '("list" "123" "list" "123" "123"))