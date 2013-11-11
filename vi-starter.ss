#lang scheme/gui

(require 
 "../sdutils/logging.ss" 
 (prefix-in  default: "vi-default-3sort-callable.ss") 
 (prefix-in  events: "vi-events-callable.ss") 
 (prefix-in  threads: "vi-threads-callable.ss")

 net/sendurl )


(define get-log-location 
  (lambda (tf e) 
    ;(display (list tf e))
    (let ((log-loc (get-directory "set log folder" d (current-directory) '())))
      (if log-loc (send tf set-value (path->string log-loc))
          (send tf set-value  "not specified")
          )
      log-loc
      )))

(define d (new frame% [label "start server"] [enabled #t] [width 500] [height 200] ))

(define log-location (simplify-path (build-path "vi-logs/")))
(unless (directory-exists? log-location) (make-directory log-location))

(define (disable-fields)
  (send d show #f)
  )



(define (start-default start-b e)
  (disable-fields)
  (default:run-vi (logfile-path "default-" log-location )))

(define (start-events start-b e)
  (disable-fields)
  (events:run-vi (logfile-path "events-" log-location)))

(define (start-threads start-b e)
  (disable-fields)
  (threads:run-vi (logfile-path "threads-" log-location)))



(define start-panel (new horizontal-panel% [parent d] [alignment '(center center)] ))
(define start-b-default (new button% [label "start default"] [parent start-panel] [callback start-default]))
(define start-b-events (new button% [label "start events"] [parent start-panel] [callback start-events]))
(define start-b-threads (new button% [label "start threads"] [parent start-panel] [callback start-threads]))
(send d  show #t)