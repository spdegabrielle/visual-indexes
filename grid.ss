#lang scheme
(require  "../sdutils/main.ss")
(provide lengths->offsets-from-origin lists-max)
;; create new list that is the cumulative sum of the source list
;; returns row/column offsets from the origin including the width of the line grid
(define (lengths->offsets-from-origin lenghts)
  (foldl (lambda (v cum-list) 
           (append cum-list (list (+ v (last cum-list)))))
         '(0) ;; start at 0
         lenghts))

(define (dimentions-table->heights-table dimentions-table)
  (map-table cadr dimentions-table))

(define (dimentions-table->widths-table dimentions-table)
  (map-table car dimentions-table))
(define (lists-max lists)
  (map (lambda (c) (apply max c)) lists))
;; the positioning offsets for each cell
; values: (offsets-for-drawing-cell-contents(x y)) table-width table-height  row-grids col-grids
(define (table-offsets dimentions-table) ;; contains the dimentions of each cell as a list (w h)
  (define heights-table (map-table cadr dimentions-table))
  (define widths-table (map-table car dimentions-table))
  ;; row and column widths and heights 
  (define max-row-heights (lists-max heights-table))
  (define max-col-widths (lists-max (transpose widths-table)))  ; transpose so 'apply max' can get the max width for the column
  ;; col and row offsets from the origin
  (define col-grids (lengths->offsets-from-origin max-col-widths))
  (define row-grids (lengths->offsets-from-origin max-row-heights))
  (values row-grids col-grids))

(define (draw-grid x y row-grids col-grids)
  ;draw horizontal and vertical lines
  (let* ((width (last col-grids))
         (height (last row-grids))
         (row-dividers
          (map ; draw horizontal row divider lines 
           (lambda (gy) ; gy is offset from y-origin for each horizontal divider line
             (list 'draw-line x (+ y gy) (+ x width) (+ y gy)))
           row-grids)) ;; row-grids is the offsets for each horizontal divider line from the y-origin
         (col-dividers
          (map ;; draw vertical column divider lines 
           (lambda (gx) ; gx is offset from x-origin for each vertical divider line
             (list 'draw-line (+ x gx) y  (+ x gx) (+ y height)))
           col-grids))) ; col-grids is the offsets for each vertical divider line from the y-origin
    (list row-dividers col-dividers)
    ))


