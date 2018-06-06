#lang racket
(require 2htdp/image)
(require 2htdp/universe)

; constant
(define HEIGHT 200) ; the height of the BACKGROUND
(define WIDTH 200) ; its width

; graphical constant
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define FOOD (circle 5 "solid" "green"))
(define WORM (circle 5 "solid" "red"))

; struct
(define-struct POS [x y])


; direct is a procedure
; w a-key -> w
; left make worm turn left
; right make worm turn right
; up make worm turn up
; down make worm turn down
(define (direct w a-key)
  (cond
    [(key=? a-key "left") w]
    [(key=? a-key "right") w]
    [(key=? a-key "up") w]
    [(key=? a-key "down") w]
    [else w]))

; worm make one step every tick 
(define (tock w) w)

; render the world on the BACKGROUND
(define (render w) w)

; judge the game is over or not
(define (stop? w) w)

; a worm and food game
(define (Worm s)
  (big-bang s
    (on-key direct)
    (on-tick tock)
    (to-draw render)
    (stop-when stop?)))