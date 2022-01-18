#lang racket
(require plot latex-pict)                         ; latex-pict is modeule to use latex
(require simple-polynomial/private/poly-smooth)   ; to make specific splines
(require simple-polynomial/fit)                   ; to make simple splines
(plot-new-window? #t)                             ; to show plot in separate window. #f is opposite
(current-directory (current-directory))           ; the current directory is the directory of rkt file 
;(current-directory "/path/to/directory")         ; for specific directory 
(define F1 (call-with-input-file* "F1.txt"
  (λ (in)
    (for/list ([l (in-lines in)])
       (map string->number (string-split l))))))  ; Fi.txt contains two column of number separated by space - x y.
                                                  ; F1 contains the same numbers but pared like (x y)
                                                  ; to see F1 use (displayln F1)
;(define spF1 (points->spline-evaluator F1))      ; make smoothed data function for plot F1, simple spline
(define spF1
  (points->spline-evaluator
   ((make-least-squares-point-smoother 7 4 0) F1))) ; make smoothed data function for plot F1, regulated form of spline

(parameterize
    [(plot-y-ticks (linear-ticks #:number 2)); Number of ticks in y axis
    (plot-line-width 4)                      ; width of axes
    (plot-font-size 26)                      ; font size fpr plot
    (plot-font-family 'modern)               ; all possibilies are here https://docs.racket-lang.org/draw/font_.html 
    (plot-tick-size 14)                      ; size of ticks
    (plot-background-alpha 0)                ; full transparancy is for 0
    (plot-x-far-axis? #t)                    ; #f - removes upper boundary of frame
    (plot-y-far-axis? #t)                    ; #f - removes right boundary of frame
    ]
  (plot
   (list                                     ; use list for more than single function to plot
    (function spF1 0 100 #:color "Royal Blue" #:width 5) ; the first function to plot
    ; all colors names are here https://docs.racket-lang.org/draw/color-database___.html
    (point-label #(1 .1) "a)" #:point-size 0))           ; second figure to plot is label a) at point (1 .1)
        ; below are options to plot at whole
        #:title (tex-math "\\mathcal{E}^{\\mathrm{gg}}_2 /\\mathcal{E}_{\\mathrm{id}}" #:scale 5) ; title of figure
                                             ;; in the latex form. Use double backslash \\ instead of single one \ 
                                             ;; scale regulates size of fonts. \Large, \Huge does not work
        #:width 800 #:height 800             ; size of plot
        #:y-min 0 #:y-max 1.5                ; range for y axes
        #:out-file "f1.pdf"                  ; save file in pdf format
        ;#:out-file "f1.png"                 ; save file in png format
        #:y-label ""                         ; there is no label at y axes
        #:x-label (tex-math "a\\ \\mathrm{(nm)}" #:scale 5)) ; x label by using latex 
  )
