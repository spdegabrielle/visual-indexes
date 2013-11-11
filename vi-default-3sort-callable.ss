#lang scheme/gui
(require ; web-server/servlet
          web-server/templates
          xml
       ;   net/sendurl
          "../mail-parse/mail-parse.ss"
          "../sdutils/main.ss"
          "../sdutils/sets.ss"
          "../sdutils/date.ss"
          "../sdutils/dumper.ss" 
          "../sdutils/flipflop.ss"
          
          "../sdutils/svg.ss"
          "vi-utils.ss"
          "vi-match-load.ss"
          
          "../sdutils/svg.ss"
          )
(define stipple-flipflop (make-flipflop "#eFeFF0" "white"))
(define vertical-spacing 20) ; the spacing between entries
(define vertical-offset 50) ; the space the list starts from the top
(define corpus-file-list (hash-map corpus (λ (path-string corpus-entry) path-string)))

(define left-margin 30)
(define dates-x-pos "30" )



;; list of unique dates extracted from corpus

(define email-dates  (hash-map corpus (λ (pathstr email) (get-from-header 'Date email))))
;(define email-dates-index (make-immutable-hash (map (λ (corpus-entry) (list (get-email-date corpus-entry) corpus-entry)) corpus)))
;; list of email dates - one for each email
;(define dates (sort (append event-dates email-dates) rfc822-date<?))

;; sort a sublist by a particular function
(define (sort-paths-by paths field-getter sort-funtion<?)
  (sort paths (λ (i j) (sort-funtion<? (field-getter i) (field-getter j)))))

(define (get-date pathstr)
  (get-from-header 'Date (hash-ref corpus pathstr)))

(define (sort-paths-by-date paths)
  (sort-paths-by paths get-date rfc822-date<?))

(define dates (sort  email-dates rfc822-date<?))
;; list 
(define emails-pathstr-by-sender
  (flatten (map (λ (sender)
                  (sort-paths-by-date 
                   (hash-ref from-index sender)
                   )
                  )
                (string-sort (hash-map from-index (λ (s paths) s ))))))
(define emails-pathstr-by-subject 
  (flatten (map (λ (subject)
                  (sort-paths-by-date 
                   (hash-ref subject-thread-index subject)
                   )
                  )
                (string-sort (hash-map subject-thread-index (λ (s paths) s ))))))
(define emails-pathstr-by-date 
  (map (λ (email-date)
         (car (hash-ref date-index email-date)));;  wont work if there is more than one email per date - fortunately emails are rarely sent at the exact same time.
       date-order ))

;; date-ypos table for positioning emails /events 

(define dates-ypos (make-offsets-list dates vertical-offset vertical-spacing))
(define (get-ypos date)
  (cadr (assoc date dates-ypos)))

(define ypos 
  (lambda (path) (void)))


;(define events-string-ypos (map (λ (event-date) (list event-date (get-ypos event-date)) ) event-dates))

;(define emails-string-ypos (map (λ (email-date) (list email-date (get-ypos email-date)) ) email-dates))
; (email-path-string->email-ypos email-path-string)
;; email-ypos pathstring
(define (email-ypos email-path)
  (get-ypos (get-from-header 'Date (hash-ref corpus email-path))))

(define (email-grid)
  (apply string-append "/n"
         (map (lambda (d y) 
                (string-append 
                 (xexpr->string 
                  `(line 
                    ((stroke ,(stipple-flipflop)) 
                     (stroke-width "16") 
                     (x1 "25") (y1 ,(number->string (+ y -4))) 
                     (x2 "940") (y2 ,(number->string (+ y -4))))))
                 "\n")
                ) (map car dates-ypos) (map (lambda (y) (cadr y)) dates-ypos) )
         ))

;; 
(define (email-line email-file-pathstr ypos)
  (let* (;(email-file-pathstr ;(car (hash-ref date-index email-date)))
         (email   (hash-ref corpus email-file-pathstr))
         (from    (truncate-string 28 (get-from-header 'From email)))
         (to      (truncate-string 28 (or (get-from-header 'To email) " ")))
         (subject (truncate-string 42 (get-from-header 'Subject email)))
         (date    (truncate-string 100  (get-from-header 'Date email)))
         (yp (number->string ypos))
         (html-email  (hash-ref eml-->html email-file-pathstr))
         ;(yp (number->string (email-ypos email-file-pathstr)))
         )
    ;(printf "aaaa~V~N" (list  from to subject date))
    (string-append 
     (xexpr->string 
      `(text ((id "t1") (stroke "none") (x "20") (y ,yp) (xml:space "preserve") )
             (a ((xlink:href ,html-email) (target "email-window"))
                (tspan ((x "100") (dy "0") (id "ts1") (font-family "sans-serif") (font-size "70%")) ,from)
                (tspan ((x "280") (dy "0") (id "ts1") (font-family "sans-serif") (font-size "70%")) ,to)
                (tspan ((x "440") (dy "0") (id "ts1") (font-family "sans-serif") (font-size "70%")) ,subject)
                (tspan ((x "680") (dy "0") (id "ts1") (font-family "sans-serif") (font-size "70%")) ,date)
                )))
     "\n" ))
  
  )


(define (email-rows-by... list-of-path-str)
  (let ((yposs (make-offsets-list list-of-path-str vertical-offset vertical-spacing)))
    (apply readable-string-join  "<!-- vi- email-rows  -->\r"
           (map (λ (path-str yp)
                  (email-line path-str yp))
                (map car yposs) (map cadr yposs)))))


(define canvas-width 1024)


(define (make-svg sorted-email-pathstrs events-file-svg)
  (call-with-output-file events-file-svg 
    (lambda (out)  
      (fprintf out "~A~N \r"
               (readable-string-join 
                prelude-svg
                (string-append
                 "width=\"" (number->string canvas-width) "\""
                 " height=\"3000\" id=\"svg2\" >")
                defs-svg
                "<!-- (c)2008 S.De Gabrielle  -->"
                ;;
                (email-grid)
                (email-rows-by... sorted-email-pathstrs)
                ;email-rows-by-date ;;
                end-svg)))
    #:mode 'text #:exists 'replace)
  ; (send-url/file events-file-svg)
  )

(make-svg emails-pathstr-by-date (build-path (current-directory) "OUT" "default-vi-date.svg"))
(make-svg emails-pathstr-by-sender (build-path (current-directory) "OUT" "default-vi-sender.svg"))
(make-svg emails-pathstr-by-subject (build-path (current-directory) "OUT" "default-vi-subject.svg"))

(define events-file-svg "OUT/default-vi-date.svg")

(define default-header-file "OUT/vi-default-header-file.svg")

(define (header-row) (include-template "templates/default/header-static.svg"))

(call-with-output-file default-header-file 
  (lambda (out) (fprintf out "~A~N \n"  (header-row) ))
  #:mode 'text #:exists 'replace)


;; construct the frame page
(define (frame-template header-file visual-index-file)
  (list #"text/html" (include-template "templates/header-frame-template.html")))

(define (start request) ;
  (frame-template default-header-file events-file-svg))


(sleep 1)
(require web-server/servlet-env)
(provide default-header-file events-file-svg run-vi)

(define (run-vi log-here)
  (serve/servlet start
                 #:launch-browser? #t
                 #:quit? #t
                 #:listen-ip #f
                 #:port 8000
                 #:extra-files-paths
                 (list (current-directory))
                 #:servlet-path "/vi-default-3sort.ss"
                 #:mime-types-path "data/mime.types"
                 #:log-file log-here
                 ;#:log-format 'parenthesized-default 
                 )
  log-here
  )