#lang scheme/gui
(require "../sdutils/date.ss"
         "../sdutils/dumper.ss"          
         "../mail-parse/mail-parse.ss"
         "../sdutils/main.ss"
         "../sdutils/sets.ss"
         "../sdutils/date.ss"
         "../sdutils/dumper.ss" 
         "../sdutils/flipflop.ss"
         "../sdutils/logging.ss"
         "../sdutils/svg.ss"
         "vi-utils.ss"
         "vi-match-load.ss"
         
         scheme/port)
(define (removecr str)
  (regexp-replace* #rx"\r\n" str "\n"))

(define (file->string f)
  (let ([sp (open-output-string)])
    (call-with-input-file f
      (λ (fp) (copy-port fp sp)))
    (get-output-string sp)))

;;; normalise-dates : email-dates-hash -> dates-emails-hash
;(define (normalise-dates email-dates-hash)
;
;                         )
(define coded-dates (make-hash))
(hash-for-each corpus (λ (file email) (hash-set! coded-dates file "")))

(define viewer (new text%))
(define app-frame (new frame% (label "code dates")))
(define email-file-message (new message% [label "FILE:"] [parent app-frame][auto-resize #t]))
(define nav-panel (new horizontal-panel% (parent app-frame) (min-height 20) (stretchable-height #f)))
(define save (new button% 
                  (label "save") 
                  (parent nav-panel) 
                  (callback (lambda (b e)
                              (save-hash coded-dates 
                                         (put-file
                                          "save-as" app-frame	 	 	 	 
                                          "data/"	 	 	 	 
                                          "coded-dates-database.sdb"	 	 	 	 
                                          "sdb"	 	 	 	 
                                          null	 	 	 	 
                                          '(("data-file" "*.sdb*")))
                                         )))))
(define app-panel (new horizontal-panel% (parent app-frame)))
(define nav-list (new list-box%	 
                      [label "email-file"][min-width 100][style '(single vertical-label)]
                      [choices (hash-map coded-dates (λ (file dates) file))] (parent app-panel)
                      (callback (lambda (b e)
                                  (let ((email-file (send b get-string-selection)))
                                    (send coded-dates-field set-value (hash-ref coded-dates email-file))
                                    (send viewer insert (removecr (file->string email-file)) 0 (send viewer last-position))
                                    (send viewer set-position 0)
                                    (send coded-dates-field focus)
                                    (send email-file-message set-label email-file)
                                    )
                                  ))))
(define content-panel   (new vertical-panel% (parent app-panel)))

(define editor (new editor-canvas% (parent content-panel)(min-height 200) (min-width 400) ))
(send editor set-editor viewer)
(define coded-dates-field (new text-field%
                               (style (list 'vertical-label 'multiple))
                               [label "dates here-> in format dd/mm/yy (multiple seperated by date)"]
                               [min-height 80] (stretchable-height #f)
                               (callback (lambda (t e) (hash-set! coded-dates (send nav-list get-string-selection) (send t get-value))))
                               [parent content-panel]))

(send app-frame show #t)