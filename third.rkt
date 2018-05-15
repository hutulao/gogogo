#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

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
(define-struct phone [area switch four])

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

;(wage* (cons (make-work 123 "Matthew" 12.95 45)
;             (cons (make-work 456 "Robby" 11.95 39)
;                   '())))

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

(define (legal? list)
  (cond
    [(and (< 0 (LOC-x (car list)) 100) (< 0 (LOC-y (car list)) 200))
     (cons  (car list) (legal (cdr list)))]
    [else (legal (cdr list))]))

(define (legal list)
  (cond
    [(empty? list) '()]
    [else (legal? list)]))
#|(legal (cons (make-LOC 1 100)
             (cons (make-LOC 50 100)
                   (cons (make-LOC 60 300) '()))))
|#

(define (check-phone num)
  (cond
    [(= (phone-area num) 713) (make-phone 281 (phone-switch num) (phone-four num))]
    [else num]))
(define (replace list)
  (cond
    [(empty? list) empty]
    [else (cons (check-phone (car list)) (replace (cdr list)))]))
;(phone-area (car (replace (cons (make-phone 713 1234 5678)
;(cons (make-phone 913 1234 5678) '())))))

(define-struct line-string [line rest])
(define-struct words-string [word rest])
(define-struct line-words-string [wl rest])

(define input (read-file "ttt.txt"))
(define input-line (read-lines "ttt.txt"))
(define input-words (read-words "ttt.txt"))
(define input-wl (read-words/line "ttt.txt"))

(define (line-convert input)
  (cond
    [(empty? input) '()]
    [else (make-line-string (car input) (line-convert (cdr input)))]))
#|(line-string-line (line-convert (read-lines "ttt.txt")))
(line-string-rest (line-convert (read-lines "ttt.txt")))
(read-lines "ttt.txt")
|#

(define (word-convert input)
  (cond
    [(empty? input) '()]
    [else (make-words-string (car input) (word-convert (cdr input)))]))
#|(words-string-word (word-convert (read-words "ttt.txt")))
(words-string-rest (word-convert (read-words "ttt.txt")))
(read-words "ttt.txt")
|#

(define (wl-convert input)
  (cond
    [(empty? input) '()]
    [else
     (make-line-words-string (word-convert (car input)) (wl-convert (cdr input)))]))
#|
(words-string-word (line-words-string-wl (wl-convert input-wl)))
(line-words-string-rest (wl-convert input-wl))
(read-words/line "ttt.txt")
|#

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))

(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else (cons (length (first lls))
                (words-on-line (rest lls)))]))
;(words-on-line lls0)
;(words-on-line lls1)

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
    (read-words/line file-name)))
;(file-statistic "ttt.txt")

(define (collapse input)
  (cond
    [(empty? input) ""]
    [else (string-append (tostring (car input)) "\n" (collapse (cdr input)))]))
(define (tostring list)
  (cond
    [(empty? list) ""]
    [else (string-append (car list)  " " (tostring (cdr list)))]))
;(collapse (read-words/line "ttt.txt"))

(define (remove-articles input)
  (cond
    [(empty? input) ""]
    [else (string-append (remove (car input)) "\n" (remove-articles (cdr input)))]))
(define (remove list)
  (cond
    [(empty? list) ""]
    [(or (string=? "a"  (car list))
         (string=? "an" (car list))
         (string=? "the" (car list))) (remove (cdr list))]
    [else (string-append (car list)  " " (remove (cdr list)))]))
;(write-file "ttt.dat" (remove-articles (read-words/line "ttt.txt")))

; 1String -> String
; converts the given 1String into a String
(define (code1 c)
  (number->string (char->integer c)))
;(code1 "A")

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(define (encode-letter s)
  (cond
    [(>= (char->integer s) 100) (code1 s)]
    [(> 10 (char->integer s)) (string-append "00" (code1 s))]
    [(> 100 (char->integer s)) (string-append "0" (code1 s))]))

(define (encode-string list)
  (cond
    [(empty? (cdr list)) (encode-letter (car list))]
    [else (string-append (encode-letter (car list)) (encode-string (cdr list)))]))

(define (encode-file input)
  (cond
    [(empty? input) ""]
    [else (string-append (encode-line (car input))
                         "\n"
                         (encode-file (cdr input)))]))
(define (encode-line list)
  (cond
    [(empty? list) ""]
    [else (string-append (encode-string (string->list (car list)))  " " (encode-line (cdr list)))]))
;(write-file "ttt.dat" (encode-file (read-words/line "ttt.txt")))

(define (wc1 input)
  (cond
    [(empty? input) '()]
    [else (string-append "words:" (word-num input)
                         "lines:" (line-num input)
                         "bytes:" (byte-num input))]))