#lang scheme/gui
(require "../mail-parse/mail-parse.ss"
         "../sdutils/main.ss"
         "../sdutils/index.ss"
         "../sdutils/date.ss"
         "../sdutils/sets.ss"
         "../sdutils/dumper.ss" 
         "vi-email.ss"
         "vi-utils.ss"
         file/md5
         ;(planet neil/csv:1:4/csv)
         (planet neil/levenshtein:1:2/levenshtein)
         "csv-to-hash.ss"
 
         )

 (csvfile->hash-file "data/events/dates.csv" "data/events/csv-dates.ss")
 
;(define-struct (path email))
(define debug #f)
(define emails-dir (build-path (current-directory) "data" "emails"))

(define file-dates-index (make-immutable-hash 
  (hash-map (load-hash 
   (build-path (current-directory) "data" "events"  	 	 
               "csv-dates.ss") ); dates-hash2.9.ss
            (λ (k v) (cons (normalise-path-string k) v)))))



(provide corpus 
         from-index to-index cc-index bcc-index agents-index
         agents ;; all email-addresses sorted list
         date-order ; sorted order list of emails date (not event)
         date-index ; email dates index
         ;dates-index ; coded event dates index
         file-dates-index ; files coded for dates
         subject-thread-index
         index
         string-sort
         horizontal-email-sep
         emails-dir
         eml-->html ;; hash
         eml->html ;; function
         )
(define horizontal-email-sep 20)

;(string-ci<? "Apple" "apple")
;;; string-sort
(define (string-sort list-of-string)
  (sort (remove-duplicates list-of-string) string-ci<?))

;; get the md5 byte string for the body of the email
;; email-body-md5 : email? -> bytes?
(define (email-body-md5 email) 
  (md5 (string->bytes/utf-8 
        (string-join (car (email-messages email)) "\n\r")))) ;; \r\n ?

(define corpus 
  (make-immutable-hash 
   (map (lambda (entry) 
          (cons (path->string (find-relative-path (current-directory) (car entry))) (caadr entry)))
        (parse-archive emails-dir)
        )))



(define (tokenise-addresses str)
  (remove-duplicates (map disamb-member (tokenise-email-addresses str))))

;(define-struct index (entry values))

;; indexes ;;
(define from-index (index (hash->list corpus) (λ (e) (tokenise-addresses (get-from-header 'From e)))))
(define to-index (index (hash->list corpus) (λ (e) (tokenise-addresses (get-from-header 'To e)))))
(define cc-index (index (hash->list corpus) (λ (e) (awhen (get-from-header 'Cc e) tokenise-addresses))))
(define bcc-index (index (hash->list corpus) (λ (e) (awhen (get-from-header 'Bcc e) tokenise-addresses))))

(define agents-index (index (hash->list corpus) (λ (e) (awhen (get-addresses e) (λ (args) args)))))
;; email-


;; dates
;; a list of paths ordered by date
;; sort-corpus-by-date : corpus -> paths 
;(define (sort-corpus-by-date corpus) ;catch
;  (sort (hash-map get-path-from corpus); list of filenames
;        short-date<?))

(define date-index (index (hash->list corpus) (λ (e) 
                                                (list (get-from-header 'Date e)))))
(define date-order (sort (hash-map date-index (λ (k v) k)) rfc822-date<?))


;;Fri, 22 Dec 2000 09:24:00 -0800 (PST)
; Wed, 20 Dec 2000 07:40 -0700 (PDT)

; email threads

(define (remove-prefix: str)
  (regexp-replace* #px"^ "   
                   (regexp-replace* #px"\\]"   (regexp-replace* #px"\\["   
                                                                (regexp-replace* #px"Fwd:"  
                                                                                 (cond 
                                                                                   [(or (equal? (substring str 0 4) "Re: ") (equal? (substring str 0 4) "FWD:"))
                                                                                    (substring str 4 (string-length str))]
                                                                                   [else str])
                                                                                 "") "") "") ""))

(define (remove-fwd: str)
  (cond 
    [(equal? (substring str (- (string-length str) 6) (string-length str)) " (fwd)")  (substring str 0 (- (string-length str) 6))]
    [else str]))

;; thread-detector : list-of-emailfiles --> listof list-of-emailfiles
;; simple thread detector 

(define (thread-detector list-of-email-files)
  (let* ((email-file-pairs (unique-pairs list-of-email-files)))
    (matching-combination 
     (filter-map 
      (lambda (eepair)
        (printf "::~V~N" (map path->string eepair))
        (if (< (string-levenshtein (remove-prefix: (get-subject (car eepair) corpus))
                                   (remove-prefix: (get-subject (cadr eepair) corpus))) 2)
            eepair #f))
      email-file-pairs))))

(define subject-thread-index (index (hash->list corpus) (λ (e) (awhen (get-from-header 'Subject e) (λ (s) (list (remove-fwd: (remove-prefix: s))))))))

;;;;;;
;an agent is any address sending or receiving an email
(define agents (sort (remove-duplicates 
                      (map disamb-member (append (hash-map from-index    (λ (k v) k))
                                                 (hash-map to-index (λ (k v) k))
                                                 (hash-map cc-index (λ (k v) k))
                                                 (hash-map bcc-index (λ (k v) k)))))
                     string<=?  ))
(define eml-->html (make-immutable-hash  
                    (hash-map  corpus  (λ (p e)  (cons p  (regexp-replace* #px"eml$" (regexp-replace* #px" " (path->string (file-name-from-path  p)) "-") "html"))))))

(define (eml->html pathstr)
  (build-path (current-directory) "OUT" (hash-ref eml-->html pathstr))
  )


(hash-for-each corpus (lambda (pathstr email) (void)
                        
                        (call-with-output-file (eml->html pathstr)
                          (lambda (out)  
                            (fprintf out "~A~N \n"
                                     (make-email-from-template  email))
                            )
                          #:mode 'text #:exists 'replace))
               ;(send-url/file events-file-svg)
               )







