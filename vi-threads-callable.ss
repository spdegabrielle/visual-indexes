#lang scheme
;#lang web-server/insta  
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
          ;"../sdutils/logging.ss" ; (logfile-path prefix (base (current-directory)))
          "../sdutils/svg.ss"
          "vi-utils.ss"
          "vi-match-load.ss"
          )
;(provide/contract (start (request? . -> . response?)))
;;; constants ;;;
(define vi-file-version ".8")
(define threads-threads-file-svg (string-append "OUT/vi2" vi-file-version ".svg")) 
(define threads-names-file-svg   (string-append "OUT/vi2" vi-file-version "-names.svg"))
;(define threads-file-html-frame  (string-append "OUT/vi2" vi-file-version ".html"))

(define offset-from-top 40)

;dialog%
;(printf "~NUser interactions logged here; ~N~V~N" (path->string log-here))

;(static-files-path (current-directory))

;;;; horizontal email arrangement of emails
;;  list of names(email-addresses) AND y values 
(define email-xpos-list (make-offsets-list 
                         (append-map (λ (date) (hash-ref date-index date)) date-order)
                         20 horizontal-email-sep)) ;;-> date - xpos


(define CANVAS-WIDTH (number->string (+ 2000 (cadr (last email-xpos-list)))))
;; index of all agents and their associated files?
(define agents-pos-list (make-offsets-list agents offset-from-top 15))
;; get-a-pos : agents-pos-list agent -> number
(define (get-agent-yposition agent)
  (cadr (assoc agent agents-pos-list)))
;; map-get-pos : agents-pos-list agents -> listof number?
(define (map-get-pos agents)
  (map (λ (agent) (get-agent-yposition agent)) agents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; name-line-xml-string : namestring vertical-position -> 
(define (name-line name ypos  stipple  [show-name #t])
  (let ((yp (number->string ypos))
        (line-yp (number->string (- ypos 8)))
        (start (number->string 0))
        (end  CANVAS-WIDTH))
    (string-append 
     (xexpr->string  `(line ((stroke ,stipple) (stroke-width "15") (x1 ,start) (y1 ,line-yp) (x2 ,end) (y2 ,line-yp))))
     "\r" 
     (if show-name
         (xexpr->string 
          `(text ((id "t1") (stroke "none") (x "20") (xml:space "preserve") (y ,yp)) 
                 (tspan ((dx "10") (dy "-5") (id "ts1") (font-family "sans-serif") (font-size "70%")) ,name)))
         " ")
     "\r"
     )))

(define stipple-flipflop
  (make-flipflop "white" "#eFeFF0"))
(define stipple-flipflop2
  (make-flipflop "white" "#eFeFF0"))

(define name-lines  (string-append
                     
                     "\n<!-- stipple background lines for names -->\n"
                     (string-join 
                      (map (lambda (name pos) (name-line name pos  (stipple-flipflop) #t));name-line ; name ypos
                           (map car agents-pos-list)
                           (map cadr agents-pos-list)
                           )
                      "\n")))

(define no-name-lines  (string-append
                        
                        "\n<!-- stipple background lines for names -->\n"
                        (string-join 
                         (map (lambda (name pos) (name-line name pos  (stipple-flipflop2) #f)); name ypos
                              (map car agents-pos-list)
                              (map cadr agents-pos-list)
                              )
                         "\n")))


;; list of ypositions for each address in an email, starting with the sender
(define (email-y-positions file)
  (let* ((email (hash-ref corpus file))
         (from (get-from-header 'From email) )
         (from-pos  (get-agent-yposition from))
         (recipients (get-recipient-addresses email))
         (agent-positions (sort (map-get-pos (intersection agents recipients)) <)))
    (cons from-pos agent-positions)))  ;put sender at the begining

;; get the smallest(top) yposition used by this email
(define (get-ytop email-file)
  (apply min (email-y-positions email-file)))
;; get the largest(bottom) yposition used by this email
(define (get-ybottom email-file)
  (apply max (email-y-positions email-file)))

;; create vertical V path movement instructions
(define (make-vert-connectors y-ext) ;
  (λ (y-base) (string-append " V " (number->string y-base) " V " (number->string y-ext) " ")))

(define (draw-thread colour stroke-opacity start-x tmin vert-connector-fn email-x-positions email-y-limit-positions)
  (string-append "<path stroke-width=\"0.5\" stroke=\"" colour "\"  stroke-opacity=\"" stroke-opacity "\"  d=\"M " start-x "," (number->string tmin); notch
                 (string-join 
                  (map (lambda (x ypos) (string-append " H " (number->string x) (vert-connector-fn ypos)))
                       email-x-positions email-y-limit-positions)
                  " ") "\" />"))

;; make-thread-svg : listof files -> string
;; test :(make-thread-svg (fourth (thread-detector emails)))
(define (make-thread-svg email-files (ystart 40) (subject-line "unset"))
  (let* ((tmin (- (apply min (flatten (map email-y-positions email-files))) (+ 30 (* 3 (random 7))))) ; top
         (tmax (+ (apply max (flatten (map email-y-positions email-files))) (+ 30 (* 3 (random 7))))) ; bottom
         (vert-min-connector-fn (make-vert-connectors tmin))
         (vert-max-connector-fn (make-vert-connectors tmax))
         (email-y-top-positions 
          (map (λ (i) (cadr i))
               (sort (map (lambda (f) (list (get-email-x-pos email-xpos-list f) (get-ytop f))) email-files) 
                     (λ (l r) (< (car l) (car r))))))
         (email-y-bottom-positions 
          (map (λ (i) (cadr i)) 
               (sort (map (lambda (f) (list (get-email-x-pos email-xpos-list f) (get-ybottom f))) email-files) 
                     (λ (l r) (< (car l) (car r))))))
         (email-x-positions (sort (map (lambda (f) (get-email-x-pos email-xpos-list f)) email-files) <))
         (start-x (number->string (car email-x-positions)))
         (style " style=\"z-index:-1;\" ")
         (tooltip 
          (string-append "\n" 
                         "<text class=\"tooltip\"  style=\"z-index:2;\" fill=\"black\" stroke=\"black\" stroke-width=\"0\" x=\"" 
                         start-x "\"  y=\"" 
                         (number->string (- tmin 10)) 
                         "\" dy=\"\60\" font-size=\"18\">"
                         subject-line
                         "</text>"
                         "\n" )) ;  
         (colour (colour))
         )
    
    
    ;; connectors
    (if  (= 1 (length email-files)) ;; single item thread
         
         (string-append   (string-append  "<g " style " id=\"thread:" subject-line "\" stroke=\"" colour "\">")
                          "\r" 
                          (email-lines email-files); list of files -> svg for each email
                          "\r" 
                          (readable-string-join tooltip "</g>" )
                          )
         ;; upper
         
         (string-append   
          (string-append  "<g " style " id=\"thread:" subject-line "\" stroke=\"" colour "\" >");;;;;;;;;;;;;;;;;;;;
          "\r" 
          (string-append "<path stroke-width=\"16\" stroke=\"pink\" stroke-opacity=\"0.0\"  d=\"M " start-x "," (number->string tmin); notch
                         (string-join 
                          (map (lambda (x ypos) (string-append " H " (number->string x) (vert-min-connector-fn ypos)))
                               email-x-positions email-y-top-positions)
                          " ") "\" />")
          "\r" 
          (string-append "<path id=\"threadtop:" colour "\" stroke-width=\"0.5\" stroke=\"" colour "\" d=\"M " start-x "," (number->string tmin); notch
                         (string-join 
                          (map (lambda (x ypos) (string-append " H " (number->string x) (vert-min-connector-fn ypos)))
                               email-x-positions email-y-top-positions)
                          " ") "\" />")
          
          "\r" 
          ;; lower
          (string-append "<path stroke-width=\"16\" stroke=\"pink\" stroke-opacity=\"0.0\"  d=\"M " start-x "," (number->string tmax) ;notch
                         (string-join 
                          (map (lambda (x ypos) (string-append " H " (number->string x) (vert-max-connector-fn ypos))) 
                               email-x-positions email-y-bottom-positions)
                          " ") "\" />")
          "\r" 
          (string-append "<path id=\"threadbottom:" colour "\" stroke-width=\"0.5\" stroke=\"" colour "\" d=\"M " start-x "," (number->string tmax) ;notch
                         (string-join 
                          (map (lambda (x ypos) (string-append " H " (number->string x) (vert-max-connector-fn ypos))) 
                               email-x-positions email-y-bottom-positions)
                          " ") "\" />")
          "\r" 
          (email-lines email-files); list of files -> svg for each email
          "\r" 
          (readable-string-join tooltip "</g>")
          ))))

(define emails-by-thread (hash-map subject-thread-index (λ (thread-subject-line email-pathstrings) (list thread-subject-line email-pathstrings))))
; (thread-detector emails-by-date))

(define (email-lines email-files); list of files -> svg for each email
  ; (dump-all email-files)
  (apply readable-string-join (map email-line email-files)))

;; ypos -> number?
;; return lower number each time called
(define ypos 
  (let ((y 80))
    (λ () (set! y (- y 5)) y)))

;; email-line : path-string move -> xml-string
;; create a single vertical line
(define (email-line path-string)
  ;(dump-all path-string)
  (let* ((email-x (get-email-x-pos email-xpos-list path-string))
         (addresses (get-agentype-address-list (hash-ref corpus path-string)))
         (ytop (- (apply min (map get-agent-yposition (append-map cadr addresses))) 8))
         (ybottom (- (apply max (map get-agent-yposition (append-map cadr addresses))) 8))
         (agent-markers ;(xml->xexpr
          (apply readable-string-join
                 (map (λ (type-addresses)
                        (let ((type-symbol (string-append "#" (symbol->string (car type-addresses)))))
                          (apply readable-string-join
                                 (map (λ (address)
                                        (let ((address-x email-x)
                                              (address-y (get-agent-yposition address)))
                                          (xexpr->string
                                           `(use ((height "16")
                                                  (width "16") 
                                                  (x ,(number->string (- address-x 8)))
                                                  (y ,(number->string (- address-y 16)))
                                                  (xlink:href ,type-symbol)))))
                                        )  (cadr type-addresses))))
                        
                        ) addresses))))
    
    ;;
    (readable-string-join 
     (string-append "<a xlink:href=\"" (hash-ref eml-->html path-string) "\" target=\"email-window\" >"
                    "\n" 
                    (xexpr->string `(line ((stroke-width "16" ) (stroke "pink") (stroke-opacity "0.0")
                                                                (y1 ,(number->string ytop))
                                                                (x1 ,(number->string email-x))
                                                                (y2 ,(number->string ybottom))
                                                                (x2 ,(number->string email-x)))))
                    "\n" 
                    (xexpr->string `(line ((stroke-width "2" ); (stroke "dimgray") 
                                           (y1 ,(number->string ytop))
                                           (x1 ,(number->string email-x))
                                           (y2 ,(number->string ybottom))
                                           (x2 ,(number->string email-x)))))
                    "\n" 
                    agent-markers ;; draw agent-markers ;from-to-cc-bcc
                    "\n" 
                    (string-append "</a>") 
                    "\n" )
     )
    ))

;; return xml for threads and emails
(define threads 
  (string-append  "<g  id=\"threads\"   fill=\"black\"  >"  "\n" ;stroke-width=\"2px\"
                  (apply 
                   readable-string-join
                   (map ;; threads
                    (λ (thread-subject-line email-pathstrings)
                      (make-thread-svg email-pathstrings (ypos) thread-subject-line))
                    (map car emails-by-thread) (map cadr emails-by-thread)))
                  "\n" 
                  
                  "</g>"))

;;;;;;
(define (make-svg content-string events-file-svg canvas-width (canvas-height 3000))
  (call-with-output-file events-file-svg 
    (lambda (out)  
      (fprintf out "~A~N \r"
               (readable-string-join 
                prelude-svg
                (string-append
                 "width=\"" (number->string canvas-width) "\""
                 " height=\"" (number->string canvas-height) "\" id=\"svg2\" >")
                defs-svg
                content-string
                end-svg)))
    #:mode 'text #:exists 'replace)
  )

(call-with-output-file threads-threads-file-svg 
  (lambda (out)  
    (fprintf out "~A~N \r"
             (readable-string-join 
              prelude-svg
              
              (string-append
               "width=\"" CANVAS-WIDTH "\""
               " height=\"635\" id=\"svg2\" >")
              
              defs-svg
              "<!-- (c)2008 S.De Gabrielle  -->"
              ; background
              ;#;(string-append "<rect x=\"0\" y=\"0\" width=\"" (number->string canvas-width)
              ;               "\" " "height=\"735\"  stroke=\"none\" fill=\"#DDFFFF\" stroke-width=\"1\" />")
              no-name-lines
              ;name-lines ;;
              threads
              ;email-lines ;;
              end-svg)))
  #:mode 'text #:exists 'replace)
(call-with-output-file threads-names-file-svg 
  (lambda (out)  
    (fprintf out "~A~N \r"
             (readable-string-join 
              prelude-svg
              "width=\"250\" height=\"635\" id=\"svg2\" >"
              
              defs-svg
              "<!-- (c)2008 S.De Gabrielle  -->"
              ; background
              ;#;(string-append "<rect x=\"0\" y=\"0\" width=\"" (number->string canvas-width)
              ;               "\" " "height=\"735\"  stroke=\"none\" fill=\"#DDFFFF\" stroke-width=\"1\" />")
              name-lines ;;
              end-svg)))
  #:mode 'text #:exists 'replace)

;; construct the frame page
(define (frame-template threads-names-file-svg threads-threads-file-svg)
  (list #"text/html" (include-template "templates/frame-template.html")))

(define (start request) ;
  (frame-template threads-names-file-svg threads-threads-file-svg))

;; create frame
;#;(call-with-output-file threads-file-html-frame 
;    (lambda (out)  
;      (fprintf out "~A~N \r"
;               (cadr (frame-template  threads-names-file-svg threads-threads-file-svg))))
;    #:mode 'text #:exists 'replace)

;(send-url/file threads-file-html-frame)

(sleep 1)
(require web-server/servlet-env)


(provide run-vi threads-names-file-svg threads-threads-file-svg
         )


(define (run-vi log-here)
  (serve/servlet start
                 #:launch-browser? #t
                 #:quit? #t
                 #:listen-ip #f
                 #:port 8082
                 #:extra-files-paths
                 (list (current-directory))
                 #:servlet-path "/vi-threads.ss"
                 #:mime-types-path "data/mime.types"
                 #:log-file log-here
                 #:log-format 'parenthesized-default 
                 )
  log-here
  )

