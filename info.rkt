#lang info
(define collection "plot-cookbook")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "infix" "threading" "math-lib" "latex-pict" "colormaps" "sci" "simple-polynomial" "metapict" "science" "data-frame" "csv-reading"))
(define scribblings '(("scribblings/plot-cookbook.scrbl" ())))
(define pkg-desc "A cookbook of recipes for creating plots with Racket's plot library")
(define version "0.0")
(define pkg-authors '(spdegabrielle))
(define license '(Apache-2.0 OR MIT))
