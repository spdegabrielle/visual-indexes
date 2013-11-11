#lang scheme
;; csv-> hash
(require "../sdutils/main.ss"
         (planet neil/csv:1:4/csv)
         )
(provide csvfile->hash-file)

(define (list->hash assocs)
  (make-immutable-hash assocs))

(define (remove-empty lists)
  (map (λ (list) 
         (filter (λ (item) (if (equal? (string-length item) 0) #f #t)) list)
         
         ) lists)
  )

(define (csvfile->hash-file csvfile hash-file)
  (call-with-input-file csvfile
    (lambda (in)
      (save-hash ;dict
       (list->hash 
        
        (remove-empty (csv->list in))
                   
                   )
       hash-file)
      ))
  )

;;tests
(define (test)
  (csvfile->hash-file "data/events/dates2.9.csv" "data/events/csv-dates.ss")
  (void)
  )

;(test)

;(csvfile->hash-file "data/events/dates2.csv" "data/events/csv-dates3.ss")