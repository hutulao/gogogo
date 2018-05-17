#lang racket
;(require )
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
; press shift to insert the circle between pre and post
(define (editor-add-pic ed)
  (make-editor (reverse (cons (circle 5 "solid" "red") (reverse (editor-pre ed))))
               (editor-post ed)))

; Editor KeyEvent -> Editor
; deals with a key event, given an editor
(define (editor-kh ed ke)
  (cond
    [(key=? ke "\b") (editor-del ed)]
    [(key=? ke "right") (editor-rgt ed)]
    [(key=? ke "left") (editor-lft ed)]
    [(key=? ke "\r") ed]
    [(key=? ke "\t") ed]
    [(key=? ke "shift") (editor-add-pic ed)]
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
;(main ">")

;(define x 'a)
;(list (quote quote) x)
;(cons (quote quote) (cons 'a '()))
;(equal? (list 0 1) (cons 0 '(1)))
;(fourth (list 1 2 4 3))

; number list-of-numbers -> list-of-numbers
; insert n into the sorted list of numbers alon
(define (insert n alon)
  (cond
    [(empty? alon) (cons n alon)]
    [(<= (car alon) n) (cons n alon)]
    [else (cons (car alon) (insert n (cdr alon)))]))

; list-of-number -> list-of-number
; rearranges alon in descending order
; produces a sorted version of alon
(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [(list? alon) (insert (car alon) (sort> (cdr alon)))]
    [else (error "alon is not list")]))

; list-of-number -> boolean
; judge alon is descending order or not
(define (sorted>? alon)
  (cond
    [(or (empty? alon) (empty? (cdr alon))) #t]
    [else (and (> (car alon) (car (cdr alon)))
               (sorted>? (cdr alon)))]))

;(check-satisfied (sort> (list 9 8 7 6 4 3 2 1 0 5))  sorted>?)

; A GamePlayer is a structure:
; (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points
(define-struct gp [name score])

(define (insert-player player list)
  (cond
    [(empty? list) (cons player '())]
    [(>= (gp-score player) (gp-score (car list))) (cons player list)]
    [else (cons (car list) (insert-player player (cdr list)))]))

(define (game-score> list)
  (cond
    [(empty? list) '()]
    [else (insert-player (car list) (game-score> (cdr list)))]))

(define (game-score>? list)
  (cond
    [(or (empty? list) (empty? (cdr list))) #t]
    [else (and (> (gp-score (car list)) (gp-score (car (cdr list))))
               (game-score>? (cdr list)))]))

;(check-satisfied (game-score> (list (make-gp "jack" 50) (make-gp "lily" 40)
;                                    (make-gp "tom" 60) (make-gp "linda" 80)))  game-score>?)

(define-struct email [from date message])

(define (insert-email email list)
  (cond
    [(empty? list) (cons email '())]
    [(string<? (email-from email) (email-from (car list))) (cons email list)]
    [else (cons (car list) (insert-email email (cdr list)))]))

(define (email-sort> list)
  (cond
    [(empty? list) '()]
    [else (insert-email (car list) (email-sort> (cdr list)))]))

(define (email-sort>? list)
  (cond
    [(or (empty? list) (empty? (cdr list))) #t]
    [else (and (string<? (email-from (car list)) (email-from (car (cdr list))))
               (email-sort>? (cdr list)))]))
;(check-satisfied (email-sort> (list (make-email "yinda" 50 100) (make-email "lily" 40 300)
;                                    (make-email "tom" 60 200) (make-email "jack" 80 400)))  email-sort>?)

; Number List-of-numbers -> Boolean
(define (search-sort n alon)
  (cond
    [(empty? alon) #f]
    [(< (first alon) n) #f]
    [else (or (= (first alon) n)
              (search-sort n (rest alon)))]))
;(search-sort 9.5 (sort> (list 9 8 7 6 4 3 2 1 10 5)))

(define (prefixes list)
  (cond
    [(empty? list) '()]
    [else (cons (reverse list) (prefixes (cdr list)))]))
(prefixes (reverse (list 9 8 7)))

(define (suffixes list)
  (cond
    [(empty? list) '()]
    [else (cons list (suffixes (cdr list)))]))
(suffixes (list 9 8 7))