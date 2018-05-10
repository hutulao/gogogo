#lang racket
(require 2htdp/universe)
(require 2htdp/image)

; distances in terms of pixels
(define HEIGHT 800) 
(define WIDTH 1000)


;some struct
(define-struct LOC [x y])
(define-struct VEL [dx dy])
(define-struct UFO [loc vel])
(define-struct TANK [loc vel])
(define-struct MISSILE [loc vel])
(define-struct SIGS [Tank UFO Missile])
(define-struct layer [color doll])

; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define MISSILE-IMAGE (triangle 5 "solid" "red"))
(define UFO-IMAGE (overlay (circle 10 "solid" "green")
                           (rectangle 30 10 "solid" "green")))
(define TANK-IMAGE (overlay/align "middle" "bottom"
                                  (circle 10 "solid" "black")
                                  (rectangle 30 10 "solid" "black")))

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
    [(= n (image-width (car list-image)) (image-height (car list-image)))
     (car list-image)]
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
                    (hall 20 10 (rectangle 10 10 "outline" "black"))
                    (empty-scene 100 200)))

(define red-dot (circle 4 "solid" "red"))
(define (add-balloons list)
  (cond
    [(empty? list) background]
    [else (place-image red-dot
                       (LOC-x (car list))
                       (LOC-y (car list))
                       (add-balloons (cdr list)))]))
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
                                                     (cons (make-LOC 100 200)
                                                           '())))))))))))
|#

(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [else (+ 1 (depth (layer-doll an-rd)))]))
;(depth (make-layer "pink" (make-layer "black" "white")))

(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (string-append (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))
;(colors (make-layer "yellow" (make-layer "green" "red")))

(define (inner an-rd)
  (cond
    [(string? an-rd) (triangle 40 "solid" an-rd)]
    [else (inner (layer-doll an-rd))]))
;(inner (make-layer "yellow" (make-layer "green" "red")))



(define (UFO-render ufo back)
  (place-image UFO-IMAGE (LOC-x (UFO-loc ufo)) (LOC-y (UFO-loc ufo)) back))
;(UFO-render (make-UFO (make-LOC 120 50) (make-VEL 1 0)) BACKGROUND)

(define (TANK-render tank back)
  (place-image TANK-IMAGE (LOC-x (TANK-loc tank)) (LOC-y (TANK-loc tank)) back))
;(TANK-render (make-TANK (make-LOC 120 20) (make-VEL 1 0)) BACKGROUND)

;invader-game is a procedure
;interpretation game invader Tank UFO Missile 
(define (invader-game ws)
  (big-bang ws
    (on-tick move)
    (on-key key-action)
    (to-draw render)
    (stop-when game-over? last-render)
    ))


;last-render is a procedure
;last ws -> image
(define (last-render s)
  (overlay (text
            (cond
              [(>= (LOC-y (UFO-loc (SIGS-UFO s))) HEIGHT) "UFO lands game is over"]
              [(Missile-hit? (SIGS-Missile s) (SIGS-UFO s)) "Mission accomplished"])
            36 "olive") BACKGROUND))


;game-over? is a procedure
;ws -> end?
;UFO lands or missile hits the UFO
;end the game
(define (game-over? s)
  (cond
    [(>= (LOC-y (UFO-loc (SIGS-UFO s))) HEIGHT) #true]
    [(Missile-hit? (SIGS-Missile s) (SIGS-UFO s)) #true]
    [else #false]))

;Missile-hit? is a procedure
;ws -> boolean
;judge missile hits the UFO
(define (Missile-hit? mi ufo)
  (cond
    [(empty? mi) #false]
    [else (or (and (>= (+ (LOC-y (UFO-loc ufo)) 2)
                       (LOC-y (MISSILE-loc (car mi)))
                       (- (LOC-y (UFO-loc ufo)) 2))
                   (>= (+ (LOC-x (UFO-loc ufo)) 2)
                       (LOC-x (MISSILE-loc (car mi)))
                       (- (LOC-x (UFO-loc ufo)) 2)))
              (Missile-hit? (cdr mi) ufo))]))
  

;key-action is a procedure
;ws key -> ws
;key TAB launch the Missile
;key left arrow make Tank left move
;key right arrow make Tank right move
;other key ws don't change
(define (key-action ws key)
  (cond
    [(key=? key "right") (make-SIGS (make-TANK (TANK-loc (SIGS-Tank ws))
                                               (make-VEL 1 0))
                                    (SIGS-UFO ws)
                                    (SIGS-Missile ws))]
    [(key=? key "left") (make-SIGS (make-TANK (TANK-loc (SIGS-Tank ws))
                                               (make-VEL -1 0))
                                    (SIGS-UFO ws)
                                    (SIGS-Missile ws))]
    [(key=? key " ") (make-SIGS (SIGS-Tank ws)
                                (SIGS-UFO ws)
                                (cons (make-MISSILE (TANK-loc (SIGS-Tank ws)) (make-VEL 0 -2))
                                      (SIGS-Missile ws)))]
    [else ws]))
#|(key-action (make-SIGS (make-TANK (make-LOC 120 20) (make-VEL 1 0))
                       (make-UFO (make-LOC 120 50) (make-VEL 1 0))
                       '()) " ")
(key-action (make-SIGS (make-TANK (make-LOC 120 20) (make-VEL 1 0))
                       (make-UFO (make-LOC 120 50) (make-VEL 1 0))
                       (cons (make-MISSILE (make-LOC 120 550) (make-VEL 1 0))
                             (cons (make-MISSILE (make-LOC 220 450) (make-VEL 1 0))
                                   (cons (make-MISSILE (make-LOC 520 650) (make-VEL 1 0)) '())))) " ")
|#



(define (Missile-move m)
  (cond
    [(empty? m) m]
    [else (check-missile m)]))

(define (check-missile m)
  (cond
    [(< (LOC-y (MISSILE-loc (car m))) 0) (Missile-move (cdr m))]
    [else
     (cons (make-MISSILE (make-LOC (LOC-x (MISSILE-loc (car m)))
                                   (+ (LOC-y (MISSILE-loc (car m)))
                                      (VEL-dy (MISSILE-vel (car m)))))
                         (MISSILE-vel (car m)))
           (Missile-move (cdr m)))]))
;(Missile-move (cons (make-MISSILE (make-LOC 120 550) (make-VEL 2 0))
;                    (cons (make-MISSILE (make-LOC 220 450) (make-VEL 2 0))
;                          (cons (make-MISSILE (make-LOC 520 650) (make-VEL 2 0)) '()))))


;move is a procedure
;ws -> ws
;move Tank UFO Missile 
(define (move ws)
  (make-SIGS (Tank-move (SIGS-Tank ws))
             (UFO-move (SIGS-UFO ws))
             (Missile-move (SIGS-Missile ws))))

(define (Tank-move tank)
  (make-TANK (make-LOC (+ (LOC-x (TANK-loc tank)) (VEL-dx (TANK-vel tank)))
                       (LOC-y (TANK-loc tank)))
             (TANK-vel tank)))
;(Tank-move (make-TANK (make-LOC 120 20) (make-VEL 1 0)))

(define (UFO-move ufo)
  (make-UFO (make-LOC (random 100 700)
                      (+ (LOC-y (UFO-loc ufo)) (VEL-dy (UFO-vel ufo))))
            (UFO-vel ufo)))
;(UFO-move (make-UFO (make-LOC 120 20) (make-VEL 1 0)))


(define (MISSILE-render m)
  (cond
    [(empty? m) BACKGROUND]
    [else (place-image MISSILE-IMAGE
                       (LOC-x (MISSILE-loc (car m)))
                       (LOC-y (MISSILE-loc (car m))) (MISSILE-render (cdr m)))]))


;(MISSILE-render (cons (make-MISSILE (make-LOC 120 550) (make-VEL 1 0))
;                      (cons (make-MISSILE (make-LOC 220 450) (make-VEL 1 0))
;                            (cons (make-MISSILE (make-LOC 520 650) (make-VEL 1 0)) '()))) BACKGROUND)


;render is a procedure
;ws -> image
;render worldstate on the BACKGROUND 
(define (render ws)
  (TANK-render (SIGS-Tank ws)
               (UFO-render (SIGS-UFO ws)
                           (MISSILE-render (SIGS-Missile ws)))))  

(invader-game (make-SIGS (make-TANK (make-LOC 120 (- HEIGHT 20)) (make-VEL 1 0))
                         (make-UFO (make-LOC 120 0) (make-VEL 0 1))  '()))
;(invader-game (make-SIGS (make-TANK (make-LOC 120 20) (make-VEL 1 0))
;                         (make-UFO (make-LOC 120 50) (make-VEL 1 0))
;                         (make-MISSILE (make-LOC 120 550) (make-VEL 1 0))))



; ShotWorld -> Image
; adds the image of a shot for each y on w
; at (MID,y) to the background image
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image MISSILE-IMAGE XSHOTS (car w) (to-image (cdr w)))]))
;(to-image '(9 20 31))

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(define (tock w)
  (cond
    [(empty? w) w]
    [else (check-shot w)]))

(define (check-shot w)
  (cond
    [(< (car w) 0) (tock (cdr w))]
    [else (cons (+ -1 (car w)) (tock (cdr w)))]))
;(tock '(1 0 -1 20 31))

; ShotWorld KeyEvent -> ShotWorld
; add a shot to the world
; if the player presses the space bar
(define (keyh w ke)
  (cond
    [(key=? " " ke) (cons HEIGHT w)]
    [else w]))
;(keyh '(9 20 31) " ")

; ShotWorld -> ShotWorld
(define (main w)
  (big-bang w
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
;(main '())


