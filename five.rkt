#lang racket
(require 2htdp/image)
(require 2htdp/universe)

; constant
(define DIAMETER 4)
(define HEIGHT (* DIAMETER 300)) ; the height of the BACKGROUND
(define WIDTH (* DIAMETER 400)) ; its width


; graphical constant
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define FOOD (circle DIAMETER "solid" "green"))
(define WORM (circle DIAMETER "solid" "red"))

; struct
(define-struct POS [x y])
(define-struct VEL [dx dy])
(define-struct worm [pos vel])

; LWorms is one of 
; '()
; (cons worm LWorms)


; direct is a procedure
; w a-key -> w
; left make worm turn left
; right make worm turn right
; up make worm turn up
; down make worm turn down
(define (direct w a-key)
  (cond
    [(and (key=? a-key "left") (not (= (VEL-dx (worm-vel w)) DIAMETER)))
     (make-worm (worm-pos w) (make-VEL (- DIAMETER) 0))]
    [(and (key=? a-key "right") (not (= (VEL-dx (worm-vel w)) (- DIAMETER))))
     (make-worm (worm-pos w) (make-VEL DIAMETER 0))]
    [(and (key=? a-key "up") (not (= (VEL-dy (worm-vel w)) DIAMETER)))
     (make-worm (worm-pos w) (make-VEL 0 (- DIAMETER)))]
    [(and (key=? a-key "down") (not (= (VEL-dy (worm-vel w)) (- DIAMETER))))
     (make-worm (worm-pos w) (make-VEL 0 DIAMETER))]
    [else w]))

; worm make one step every tick 
(define (tock w)
  (make-worm (make-POS (+ (VEL-dx (worm-vel w)) (POS-x (worm-pos w)))
                       (+ (VEL-dy (worm-vel w)) (POS-y (worm-pos w))))
             (worm-vel w)))

; render the world on the BACKGROUND
(define (render w)
  (place-image WORM (POS-x (worm-pos w)) (POS-y (worm-pos w)) BACKGROUND))
;(render (make-worm (make-POS 100 190) (make-VEL 0 0)))

; judge the game is over or not
(define (stop? w)
  (cond
    [(< (POS-x (worm-pos w)) 0) #t]
    [(> (POS-x (worm-pos w)) WIDTH) #t]
    [(> (POS-y (worm-pos w)) HEIGHT) #t]
    [(< (POS-y (worm-pos w)) 0) #t]
    [else #f]))

; render the last world
(define (last-render w)
  (overlay/align "middle" "bottom" (text "game over!" 24 "olive")
                 (place-image WORM (POS-x (worm-pos w)) (POS-y (worm-pos w)) BACKGROUND)))

; a worm and food game
(define (worm-main s)
  (big-bang s
    (on-key direct)
    (on-tick tock)
    (to-draw render)
    (stop-when stop? last-render)))
(worm-main (make-worm (make-POS 400 400) (make-VEL 0 0)))