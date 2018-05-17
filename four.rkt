#lang racket
;(require lang/htdp-advanced)
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

; editor -> editor
; delete a 1string in the editor-pre
(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor (cdr (editor-pre ed)) (editor-post ed))]))
;(editor-pre (editor-del (make-editor '("1") '("1" "2"))))
;(editor-post (editor-del (make-editor '("1") '("1" "2"))))

; editor -> editor
; editor-lft move cursor a left step 
(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
     (make-editor (cdr (editor-pre ed))
                  (cons (car (editor-pre ed)) (editor-post ed)))]))
;(editor-pre (editor-lft (make-editor '("1") '("2" "3" "4"))))
;(editor-post (editor-lft (make-editor '("1") '("2" "3" "4"))))

; editor -> editor
; editor-rgt move cursor a right step 
(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else
     (make-editor (reverse (cons (car (editor-post ed))
                                 (reverse (editor-pre ed))))
                  (cdr (editor-post ed)))]))
;(editor-pre (editor-rgt (make-editor '("1") '("2" "3" "4"))))
;(editor-post (editor-rgt (make-editor '("1") '("2" "3" "4"))))

; Editor KeyEvent -> Editor
; insert the 1String k between pre and post
(define (editor-ins ed k)
  (make-editor (reverse (cons k (reverse (editor-pre ed))))
               (editor-post ed)))
;(editor-pre (editor-ins (make-editor '("1") '("2" "3" "4")) (circle 10 "solid" "red")))
;(editor-post (editor-ins (make-editor '("1") '("2" "3" "4")) (circle 10 "solid" "red")))

; Editor KeyEvent -> Editor
; deals with a key event, given an editor
(define (editor-kh ed ke)
  (cond
    [(key=? ke "\b") (editor-del ed)]
    [(key=? ke "right") (editor-rgt ed)]
    [(key=? ke "left") (editor-lft ed)]
    [(key=? ke "\r") ed]
    [(key=? ke "\t") ed]
    [(= (string-length ke) 1) (editor-ins ed ke)]
    [else ed]))

; los los -> editor
; consume two stirngs and produce a editor
(define (create-editor pre post)
  (make-editor (cons pre '()) (cons post '())))

; Editor -> image
; renders a list as an image
(define (list-render e)
  (cond
    [(empty? e) (text "" FONT-SIZE FONT-COLOR)]
    [(image? (car e)) (beside (car e) (list-render (cdr e)))]
    [else (beside (text (car e) FONT-SIZE FONT-COLOR) (list-render (cdr e)))]))

; Editor -> image
; renders an editor as an image of the two texts
; separated by the cursor
(define (editor-render e)
  (place-image/align
   (beside (list-render (editor-pre e)) CURSOR (list-render (editor-post e)))
   1 1  "left" "top" MT))

; main : String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]))
(main "")
;(main (circle 10 "solid" "red"))
;(define x 'a)
;(list (quote quote) x)
;(cons (quote quote) (cons 'a '()))