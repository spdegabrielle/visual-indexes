#lang scheme/gui
(require web-server/templates
         "../mail-parse/mail-parse.ss")
;; module to return an email in html to be served up in a named target page
(provide make-email-from-template)

(define (make-email-from-template email)
  (let ((Subject (get-from-header 'Subject email))
        (From (get-from-header 'From email)) 
        (Date (get-from-header 'Date email)) 
        (To (get-from-header 'To email)) 
        (Cc (get-from-header 'Cc email)) 
        (Bcc (get-from-header 'Bcc email))
        (Body 
         (let* ((messages (email-messages email)))
           (string-join (car messages) "<br />\n")))
        )
    (include-template "templates/email-template.html")))

