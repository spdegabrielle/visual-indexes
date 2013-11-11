#lang scheme/gui
(require  web-server/servlet
          web-server/templates
          ;web-server/dispatchers/dispatch-log
          scheme/gui ;
          xml
          net/sendurl
          scheme/date
          "../mail-parse/mail-parse.ss"
          "../sdutils/main.ss"
          "../sdutils/sets.ss"
          "../sdutils/date.ss"
          "../sdutils/dumper.ss" 
          "../sdutils/flipflop.ss"
   ;       "../sdutils/logging.ss"
          "../sdutils/svg.ss"
          "vi-utils.ss"
          "vi-match-load.ss"
          
          )

;(define log-here (logfile-path "logs/vi-events" (current-directory)))

(define stipple-flipflop (make-flipflop "#eFeFF0" "white"))
(define vertical-spacing 20) ; the spacing between entries
(define vertical-offset 5) ; the space the list starts from the top
(define corpus-file-list (hash-map corpus (λ (path-string corpus-entry) path-string)))

(define date-col 30)
(define dates-x-pos "30" )
(define from-x-pos "100" )
(define to-x-pos "280" )
(define subject-x-pos "440" )
(define event-x-pos "840" )

(define events-x-pos "830" )
(define emails-x-pos "630" )

;; list of unique dates extracted from corpus
(define event-dates (sort (remove-duplicates (flatten (hash-map file-dates-index (λ (email-path-string event-dates-list) event-dates-list))))  short-date<? ))

;(define event-dates (hash-map dates-index (λ (event-date email-path-strings) (simple-date->rfc822datestring event-date))))
;(define event-dates-path (hash-map dates-index (λ (event-date email-path-strings) (list event-date email-path-strings))))

(define email-dates  (hash-map corpus (λ (pathstr email) (get-from-header 'Date email))))

;; union of at least 2 sets 
(define (my-union set1 set2 (same? equal?))
  (remove-duplicates (append set2 set1) same?))

(define (dates-equal? d1 d2)
  (equal? (rfc822datestring->simple-date d1) (rfc822datestring->simple-date d2)))

(define (my-dates-union? event-dates email-dates)
  (my-union event-dates email-dates dates-equal?))

(define events-not-sharing-a-date-with-an-email
  (filter-not
   (λ (event-date) (ormap
                    (λ (email-date) 
                      (equal?  event-date email-date)                      
                      )
                    (map rfc822datestring->simple-date email-dates))
     )
   event-dates)
  )
;; list of email dates - one for each email
(define dates (sort (append email-dates (map simple-date->rfc822datestring events-not-sharing-a-date-with-an-email)) rfc822-date<?))


;; a unique slot for each entry in each date
;; a slot for each date, plus a slot for each email on that date/

;; remove email-dates from event dates- leaving event dates that are not email dates
(define date-sets (partition-set dates rfc822datestring->simple-date))
(set! date-sets (map (λ (date-set) (sort date-set rfc822-date<?)) date-sets))
(set! date-sets (sort date-sets (λ (ds1 ds2) (rfc822-date<? (car ds1) (car ds2)))))

(define (top-bottom num-list)
  ;(printf "aaa~V~N" num-list)
  (let* ((sortednumlist (sort num-list <))
         (top (car sortednumlist))
         (bottom (last sortednumlist))
         )
    (list top bottom)
    ))



;; date-ypos table for positioning emails /events 
(define dates-ypos (make-offsets-list dates ;(flatten date-sets) 
                                      vertical-offset vertical-spacing))
(define (get-ypos date) 
  ;(newline)(display date)
  (cadr (assoc date dates-ypos)))

(define emails-string-ypos (map (λ (email-date) (list email-date (get-ypos email-date)) ) email-dates))

;(define events-string-ypos (map (λ (event-date) (list event-date (get-ypos event-date)) ) event-dates))

(define events-string-ypos 
  (map (λ (event-date)
         (if (member event-date events-not-sharing-a-date-with-an-email)
             (list event-date (list (get-ypos (simple-date->rfc822datestring event-date)) (+ 16 (get-ypos (simple-date->rfc822datestring event-date)))))
             (list
              event-date
              (top-bottom 
               (map (λ (date) (get-ypos date))
                    (filter (λ (email-date) (equal? event-date
                                                    (rfc822datestring->simple-date email-date))) email-dates)
                    )))))
       event-dates))

;(define emails-string-ypos (map (λ (email-date) (list email-date (get-ypos email-date)) ) email-dates))
; (email-path-string->email-ypos email-path-string)
;; email-ypos pathstring
(define (email-ypos email-path)
  (get-ypos (get-from-header 'Date (hash-ref corpus email-path))))

(define (event-ypos date)
  (get-ypos date))

;; return an SVG rectangle
(define (rectangle top left bottom right (fill "#eFeFF0"))
  (let ((y (number->string top))
        (x (number->string left))
        (height (number->string (- bottom top)))
        (width (number->string (- right left))))
    (string-append 
     "<rect x=\"" x "\" y=\"" y "\" "
     "width=\"" width "\" height=\"" height "\"  "
     "fill=\"" fill "\"/>"))) ; stroke=\"#eFeFF0\" stroke-width=\"1\

;; 
(define (dates-col)
  (string-append "<g id=\"dates-col\">"
                 (apply string-append "\n"
                        (map (lambda (simple-date y-pos) 
                               (include-template "templates/svg/date.svg"))
                             (map (λ (d) (rfc822datestring->simple-date (car d))) dates-ypos) ;; dates
                             (map cadr dates-ypos) ;; y-positions
                             ))
                 "</g>"
                 ))


;;; email-yposes-for-event : event-string -> listof email-ypos for given event
;(define (email-connector-line-yposes-for-event event-string)
;  (display event-string)
;  (map (λ (email-path-string) ;; only email-ypos for this event
;         (number->string (- (email-ypos email-path-string) 4)))
;       (hash-ref dates-index event-string); -> list of email pathstrings
;       ))


(define (average positions)
  (/ (apply + positions) (length positions))
  )

(define eventdates (map (λ (es) (car es)) events-string-ypos))

;; 
(define events-ypositions (make-hash))
  
(define event-labels
  (apply string-append
         (map  (λ (event-string ypos)
                 (hash-set! events-ypositions event-string ypos)
                 (include-template "templates/svg/events-event-date.svg"))
               eventdates
               (map (λ (yp) (number->string 
                             (let ((positions (cadr yp)))
                               (if (number? positions)  (cadr yp)
                                   (average positions)
                                   ))
                             )) events-string-ypos)
               )))

;; -  event-connectors
(define event-connectors
(flatten 
  (hash-map file-dates-index (λ (filestr dates)
         (map (λ (date)
                   (string-append "<line  stroke=\"black\" stroke-width=\"0.5\" x1=\"830\" y1=\""  
                                  (hash-ref events-ypositions date)
                                  "\" x2=\"630\" y2=\""
                                  (number->string (email-ypos filestr)) 
                                  "\" />\n")
                   )
                   dates)))
  ))


;; (rectangle top left bottom right) stipple-flipflop
(define event-stipple
  (apply string-append
         (map  (λ (event-string ypos)
                 (include-template "templates/svg/events-event-date.svg"))
               eventdates
               (map (λ (yp) (number->string 
                             (let ((positions (cadr yp)))
                               (if (number? positions)  (cadr yp)
                                   (average positions)
                                   ))
                             )) events-string-ypos)
               )))



(define email-rows
  (apply readable-string-join  "<!-- vi- email-rows  -->\n"
         (map (λ (email-date); email-ypos);
                (let* ((email-file-pathstr (car (hash-ref date-index email-date)))
                       (email   (hash-ref corpus email-file-pathstr))
                       (from    (truncate-string 28 (get-from-header 'From email)))
                       (to      (truncate-string 28 (or (get-from-header 'To email) " ")))
                       (subject (truncate-string 42 (get-from-header 'Subject email)))
                       (date    (truncate-string 100  (get-from-header 'Date email)))
                       (yp (number->string (email-ypos email-file-pathstr)))
                       (html-email  (hash-ref eml-->html email-file-pathstr))
                       )
                  ;(printf "aaaa~V~N" (list  from to subject date))
                  (string-append (include-template "templates/svg/events-email-row.svg") "\n" )
                  )) date-order )))

(define canvas-width 1024)

(define events-file-svg "OUT/vi-events3.1.svg")
(define header-file "OUT/vi-events-header-file.svg")

(define (header-row) 
  (include-template "templates/svg/events-header-static.svg"))

(call-with-output-file header-file 
  (lambda (out) (fprintf out "~A~N \n"  (header-row) ))
  #:mode 'text #:exists 'replace)


;;;;
(call-with-output-file events-file-svg 
  (lambda (out)  
    (fprintf out "~A~N \r"
             (svg (string-append
                   defs-svg
                  event-labels
                  (apply string-append event-connectors)
                   (dates-col)
                   email-rows ;;
                   
                   
                   )
                  (number->string canvas-width)
                  "3000")
             ))
  #:mode 'text #:exists 'replace)

;(send-url/file events-file-svg)



(define (frame-template header-file visual-index-file)
  (list #"text/html" (include-template "templates/header-frame-template.html")))

(define (start request) 
  (frame-template  header-file events-file-svg))


(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #t
               #:quit? #t
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths
               (list (current-directory))
               #:servlet-path "/vi-events.ss"
               #:mime-types-path "data/mime.types"
               #:log-file log-here
               #:log-format 'parenthesized-default 
               )
