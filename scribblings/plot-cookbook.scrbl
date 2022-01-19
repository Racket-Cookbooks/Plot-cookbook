#lang scribble/manual
@require[@for-label[racket/base]]
@(require "common.rkt")

@title[#:tag "top"]{Plot Cookbook}
@author{Editor:spdegabrielle/various authors}

@defmodule[plot-cookbook]

The Plot library provides a flexible interface for producing nearly any kind of plot.
It includes many common kinds of plots already, such as scatter plots, line plots, contour plots, histograms, and 3D surfaces and isosurfaces.
Thanks to Racket's excellent multiple-backend drawing library, Plot can render plots as interactive snips in DrRacket, as picts in slideshows, as PNG, PDF, PS and SVG files, or on any device context.

The Plot Cookbook aims to provide an set of examples for education and resuse.

For more about Plot see @racketmodname[plot].


@table-of-contents[]

@include-section["sines.scrbl"]

@include-section["violin.scrbl"]

@include-section["basic-cook.scrbl"]

@close-plot-eval[]

@; Needs a timeout for testing:
@(module* test racket/base
   (require (submod ".."))
   (module config info
     (define timeout 180)))