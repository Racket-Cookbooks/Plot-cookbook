#lang racket/base

(require scribble/eval
         (for-label racket
                    racket/gui/base
                    pict
                    db
                    plot
                    plot/utils
                    plot/snip
                    (only-in racket/sequence sequence/c)))

(provide (all-defined-out)
         (all-from-out scribble/eval)
         (for-label (all-from-out racket
                                  racket/gui/base
                                  pict
                                  db
                                  plot
                                  plot/snip
                                  plot/utils)
                    sequence/c))

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         (prefix-in s. scribble/manual)
         (only-in racket/contract any/c)
         (for-label (only-in racket/contract any/c)))

(define (author-email) "neil.toronto@gmail.com")

(define (plot-name) "Plot")

(define plot-eval
  (let ([eval  (make-base-eval)])
    (eval '(begin
             (require racket/math racket/match racket/list racket/draw racket/class
                      plot/pict
                      plot/utils)))
    eval))

(define (close-plot-eval)
  (close-eval plot-eval))

(require plot/no-gui plot/utils pict racket/match racket/class racket/draw)
(define (pretty-print-color-maps (width 400) (height 30))
  (define cm-names
    (sort (color-map-names)
          (lambda (a b)
            (string<=? (symbol->string a) (symbol->string b)))))
  (define cm-labels
    (for/list ([cm cm-names])
      (text (symbol->string cm) null 16)))
  (define cm-picts
    (for/list ([cm cm-names])
      (parameterize ([plot-pen-color-map cm])
        (define w (/ width (color-map-size cm)))
        (apply
         hc-append 0
         (for/list ([c (in-range (color-map-size cm))])
           (match-define (list r g b) (->pen-color c))
           (define color (make-object color% r g b))
           (filled-rectangle w height #:draw-border? #f #:color color))))))
  (define picts
    (let loop ([result '()]
               [labels cm-labels]
               [picts cm-picts])
      (if (null? labels)
          (reverse result)
          (loop (cons (car picts) (cons (car labels) result))
                (cdr labels)
                (cdr picts)))))
  (table 2 picts lc-superimpose cc-superimpose 15 3))