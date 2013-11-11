#lang scheme
(require  web-server/servlet
          web-server/templates
          scheme/date
          scheme/gui
          )
(provide/contract (start (request? . -> . response?)))

(define (get-logfile-path)  
  (build-path 
   (get-directory "Specify the log folder" #f (current-directory)  '() )
   (string->path (string-append "thread-" 
                                                                (number->string (date-year (seconds->date (current-seconds)))) "-"
                                                                (number->string (date-month (seconds->date (current-seconds)))) "-"
                                                                (number->string (date-day (seconds->date (current-seconds)))) "_"
                                                                (number->string (date-hour (seconds->date (current-seconds)))) "-"
                                                                (number->string (date-minute (seconds->date (current-seconds)))) "-"
                                                                (number->string (date-second (seconds->date (current-seconds)))) ".log"
                                                                
                                                                ))))

(define logfile-path (get-logfile-path)  )
  
  
  (define (start request)
    `(html (head (title "Hello world!"))
           (body (p "Hey out there!"))))
  
  
  
  (require web-server/servlet-env)
  (serve/servlet start
                 #:launch-browser? #t
                 #:quit? #f
                 #:listen-ip #f
                 #:port 8000
                 #:servlet-path "/test-logging.ss"
                 #:log-file logfile-path  
                 #:log-format 'parenthesized-default 
                 )
  
  ;---