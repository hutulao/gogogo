#lang racket
;(require )
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/itunes)

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
;(prefixes (reverse (list 9 8 7)))

(define (suffixes list)
  (cond
    [(empty? list) '()]
    [else (cons list (suffixes (cdr list)))]))
;(suffixes (list 9 8 7))

; a Polygon is one of: 
; – (cons Posn (cons Posn (cons Posn '()))) 
; – (cons Posn Polygon)

; a plain background image 
(define BGMT (empty-scene 50 50))
(define-struct point [x y])

; Image Posn Posn -> Image 
; renders a line from p to q into img
(define (render-line img p q)
  (scene+line
   img
   (point-x p) (point-y p) (point-x q) (point-y q)
   "red"))
;(check-expect (render-line BGMT (make-posn 20 10) (make-posn 20 20))
; (scene+line BGMT
;             (posn-x (make-posn 20 10)) (posn-y (make-posn 20 10))
;             (posn-x (make-posn 20 20)) (posn-y (make-posn 20 20))
;             "red"))

(define triangle-p
  (list
    (make-point 20 10)
    (make-point 20 20)
    (make-point 30 20)))
		
(define square-p
  (list
    (make-point 10 10)
    (make-point 20 10)
    (make-point 20 20)
    (make-point 10 20)))

; Image Polygon -> Image 
; adds an image of p to img
(define (connet-dots img lod)
  (cond
    [(empty? (cdr lod)) img]
    [else
     (render-line (connet-dots img (cdr lod))
                 (first lod) (second lod))]))

; Image Polygon -> Image
; renders the given polygon p into img
(define (render-poly img p)
  (render-line (connet-dots img p) (car p) (car (reverse p))))

(define (render-poly.v1 img p)
  (connet-dots img (reverse (cons (car p) (reverse p)))))
(define (render-poly.v2 img p)
  (connet-dots img (cons (car (reverse p)) p)))

;(equal? (render-poly BGMT square-p) (render-poly.v1 BGMT square-p))
;(equal? (render-poly BGMT triangle-p) (render-poly.v2 BGMT triangle-p))

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a list-of strings
(define AS-LIST (read-lines LOCATION))

(define (charlist->stringlist list)
  (cond
    [(empty? list) '()]
    [else (cons (string (car list)) (charlist->stringlist (cdr list)))]))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (charlist->stringlist (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))

(define (start-with# letter los)
  (cond
    [(empty? los) 0]
    [(string>? (car (charlist->stringlist (string->list (car los)))) letter) 0]
    [(string<? (car (charlist->stringlist (string->list (car los)))) letter) (start-with# letter (cdr los))]
    [else (+ 1 (start-with# letter (cdr los)))]))
;(start-with# "B" AS-LIST)
(define-struct  Letter-Counts [letter counts])
(define (count-by-letter lol los)
  (cond
    [(empty? lol) '()]
    [else (cons (start-with# (car lol) los) (count-by-letter (cdr lol) los))]))
;(count-by-letter LETTERS AS-LIST)
(define (sum list)
  (cond
    [(empty? list) 0]
    [else (+ (car list) (sum (cdr list)))]))
;(sum (count-by-letter LETTERS AS-LIST))


; An LTracks is one of:
; – '()
; – (cons Track LTracks)
 
;(define-struct track
;  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's 
; title, its producing artist, to which album it belongs, 
; its playing time in milliseconds, its position within the 
; album, the date it was added, how often it has been 
; played, and the date when it was last played
 
;(define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive), 
; day (between 1 and 31), hour (between 0 
; and 23), minute (between 0 and 59), and 
; second (also between 0 and 59).

; Any Any Any Any Any Any Any Any -> Track or #false
; creates an instance of Track for legitimate inputs
; otherwise it produces #false
;(define (create-track name artist album time track# added play# played)
;  (make-track name artist album time track# added play# played))
 
; Any Any Any Any Any Any -> Date or #false
; creates an instance of Date for legitimate inputs 
; otherwise it produces #false
;(define (create-date y mo day h m s)
;  (make-date y mo day h m s))
 
; String -> LTracks
; creates a list-of-tracks representation from the
; text in file-name (an XML export from iTunes)
;(define (read-itunes-as-tracks file-name)
;  ...)

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))
;itunes-tracks
(define (total-time list)
  (cond
    [(empty? list) 0]
    [else (+ (track-time (car list)) (total-time (cdr list)))]))
;(total-time itunes-tracks)
(define (select-all-album-titles list)
  (cond
    [(empty? list) '()]
    [else (cons (track-album(car list)) (select-all-album-titles (cdr list)))]))
;(select-all-album-titles itunes-tracks)

(define (create-set-once los)
  (cond
    [(empty? los) '()]
    [(empty? (cdr los)) los]
    [(string=? (car los) (second los)) (create-set-once (cons (car los) (cdr (cdr los))))]
    [else (cons (car los ) (create-set-once (cdr los)))]))

; string list-of-strings -> list-of-strings
; insert str into the sorted list of strings list
(define (insert-los> str list)
  (cond
    [(empty? list) (cons str list)]
    [(string>? str (car list)) (cons str list)]
    [else (cons (car list) (insert-los> str (cdr list)))]))

; list-of-string -> list-of-string
; rearranges los in descending order
; produces a sorted version of los
(define (sortstr> los)
  (cond
    [(empty? los) '()]
    [(empty? (cdr los)) los]
    [else (insert-los> (car los) (sortstr> (cdr los)))]))
;(create-set-once (sortstr> '("los" "xos" "os" "xos" "alos" "os")))

(define (insert-str str list)
  (cond
    [(empty? list) (cons str list)]
    [(string=? (car list) str) list]
    [else (cons (car list) (insert-str str (cdr list)))]))
;(insert-str "e" '("d" "b" "c" "a"))
;(insert-str "c" '("d" "b" "c" "a"))

(define (get-once-list alos)
  (cond
    [(empty? alos) '()]
    [else (insert-str (car alos) (get-once-list (cdr alos)))]))
;(get-once-list (reverse '("a" "d" "c" "d" "b" "f" "a" "b")))
;(get-once-list '("a" "d" "c"))