#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

; constant
(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

; graphical constant
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; an editor is a structure:
; (make-editor los los)
; los is one of:
; - '()
; - (cons (string los))
(define-struct editor [pre post])


; Editor -> image
; renders an editor as an image of the two texts
; separated by the cursor
(define (editor-render e) MT)

; Editor KeyEvent -> Editor
; deals with a key event, given an editor
(define (editor-kh ed ke) ed)

(define (create-editor pre post)
  (cond
    [else 0]))

; main : String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]))

(define x 'a)
(list (quote quote) x)
(cons (quote quote) (cons 'a '()))