#lang scheme/gui
(require ;xml
         ;net/sendurl
         scheme/port
         "../mail-parse/mail-parse.ss"
         "../sdutils/main.ss"
        ;"../sdutils/sets.ss"
        ;"../sdutils/date.ss"
        ; "../sdutils/dumper.ss" 
                   "../sdutils/logging.ss"
         ;(planet "levenshtein.ss" ("neil" "levenshtein.plt" 1 1))
         )

(provide 
 css
 hover-style-css-path
 read-all
 make-offsets-list
 get-email-from
 get-email-date
 get-path-from
 get-from
 get-email-from-corpus
 get-subject
 corpus-list->corpus-hash!
 address-strings->addresses
 
 disamb-member
 
 get-header-fields-values
 get-recipient-addresses
 get-addresses
 get-agentype-address-list
 colour
 get-email-x-pos
 prelude-svg
 defs-svg
 end-svg
 truncate-string
 log-here
 ;;
 normalise-path-string
 )



;;; Helpers ;;;

 (define log-here (logfile-path (string-append "vi-thread") (build-path (current-directory) "logs")))

(define css " #threads path {stroke:black; stroke-width: 1px; } ")
(define hover-style-css-path (build-path (current-directory) "templates" "svg" "hover-style.css"))

(define colour
  (let* ((c (vector "maroon" "hotpink" "forestgreen" "purple" "fuchsia" "orange" "cyan" "mediumblue" "red" "yellowgreen" "darkviolet"))
         (l (vector-length c))
         (p 0))
    (lambda () (set! p  (modulo (+ p 1) l)) (vector-ref c p)))
  )
(define (truncate-string w str)
  (if (> (string-length str) w)
      (substring str 0 w)
      str))

(define (read-all fp)
  (let ([sp (open-output-string)])
    (copy-port fp sp)
    (get-output-string sp)))


;; make-offsets-list : entities [offset] [increment] -> listof pair?
;; take an ordered list of entities and return a list of pairs entity and value incrementing. 
(define (make-offsets-list entities (position 0) (increment 30))
  (map (lambda (entity-string)
         (begin (set! position (+ position increment)) 
                (list entity-string position)))
       entities))

;;;;;;;;;;;;;;;

;; corpus list item processing tools
(define (get-email-from corpus-entry)  (cadr corpus-entry))
(define (get-path-from corpus-entry)  (car corpus-entry))
(define (get-from corpus-entry) (get-from-header 'From (get-email-from corpus-entry)))
(define (get-email-date corpus-entry) (get-from-header 'Date (get-email-from corpus-entry)))
(define (get-email-from-corpus corpus email-path) (cadr (assoc email-path corpus))); corpus path -> email?


;; get-subject : file -> string
(define (get-subject email-file corpus)
  (get-from-header 'Subject (get-email-from-corpus corpus email-file)))

;; construct an index which has a list of values for each key
(define (corpus-list->corpus-hash! corpus-list corpus-hash kfn vfn)
  (for-each (lambda (p) 
              (hash-cons! corpus-hash (kfn p) (vfn p)))
            corpus-list))


(define (tokenise-addresses str)
  (remove-duplicates (map disamb-member (tokenise-email-addresses str))))


;; list -> list
(define (address-strings->addresses address-strings)
  (remove-duplicates 
   (map disamb-member (apply append 
                             (map (lambda (i) 
                                    (tokenise-addresses  i))
                                  address-strings
                                  )))))

;; get-header-fields-values : email list-of-header-fields -> list of strings
(define (get-header-fields-values email list-of-header-fields)
  (map (lambda (field) (get-from-header field email)) list-of-header-fields))

;; get all RECIPIENT addresses referred to in an email message
;; get-recipient-addresses email -> addresses
(define (get-recipient-addresses email)
  (address-strings->addresses 
   (filter (lambda (x) x) (get-header-fields-values email '(To Cc Bcc)))))
;; get all addresses referred to in an email message
;; get-addresses email  -> addresses
(define (get-addresses email)
  (address-strings->addresses
   (filter (lambda (x) x) (get-header-fields-values email '(To From Cc Bcc)))))

;; convert to definitative version
(define (disamb-member item)
  (cond 
    ((ormap (lambda (i) (equal? i item)) '("bambos@stanford.edu" "bambos@stanford.stanford.edu")) 
     "bambos@stanford.edu") 
    ((ormap (lambda (i) (equal? i item)) '("vince.j.kaminski@enron.com" "j.kaminski@enron.com" "vkamins@enron.com"  "vince.kaminski@enron.com")) 
     "vince.kaminski@enron.com")
    ((ormap (lambda (i) (equal? i item)) '("tom.gros@enroncommunications.enron.net" "tom.gros@enron.com" "thomas.gros@enron.com"))
     "tom.gros@enron.com")
    (else item))
  )
;; get all addresses referred to in an email message
;; get-agentype-address-list emaill  -> listof (type addresses)
(define (get-agentype-address-list email)
  (let* ((seen null))
    (map 
     (lambda (type) 
       (list type   
             (address-strings->addresses
              (filter (lambda (x) 
                        ; (display (list "Seen:" seen "-- @ value:"x)) (newline)
                        (if (and x (not (member (disamb-member x) seen)))
                            (begin
                              (set! seen (cons (disamb-member x) seen))
                              #t)
                            #f))
                      (remove-duplicates (apply tokenise-email-addresses (get-header-fields-values email (list type))))))))
     '(From To Cc Bcc))))


;; 
(define (get-email-x-pos email-pos-list path)
  (cadr (assoc path email-pos-list)))

(define prelude-svg  (call-with-input-file (build-path (current-directory) "templates" "prelude.svg") read-all))
(define defs-svg  (call-with-input-file (build-path (current-directory) "templates" "defs.svg") read-all))
(define end-svg "</svg>")



(define date-regex 
  (pregexp  ;; keywords/patterns
   "(?i:today|tomorrow|yesterday|last week|last month|next week|next month|dd/mm/yy|dd/mm|meeting|monday|tuesday|wednesday|thursday|friday|saturday|sunday|1st|2nd|3rd|\\d{1,2}th|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)" )) 


; regex to capture date-like entities
;; test (regexp-match* date-regex "In pattern, a start-of-string ^ refers to the first position of input after start-pos, and the end-of-input $ tueday refers to MAY the end-posth position or (in the case of an input port) the end of file, whichever comes 7th first meeting.")
;; possible-dates? : string -> listof String or #f
(define (possible-dates? string)
  (regexp-match* date-regex string)
  )

(define (normalise-path-string p)
  (path->string (simplify-path p)))
