#lang scribble/manual
@(require "common.rkt" plot threading data-frame)

@(define plot-eval-df
  (let ([eval  (make-base-eval)])
    (eval '(begin
             (require racket/math racket/match racket/list racket/draw racket/class
                      plot/pict
                      plot/utils
		      data-frame)))
    eval))

@title{Basic Recipes}

Inspired by the @(hyperlink "https://github.com/PacktPublishing/Matplotlib-3.0-Cookbook"
"Matplotlib Cookbook"), this section contains 
some recipes that begin at the very beginning.

@section{Two Points}

A line through (0, 1.5) and (1, 3).

@interaction[#:eval plot-eval
                    (eval:alts (require plot/pict) (void))
(plot
 (lines (list (vector 0 1.5)
              (vector 1 3.0))))]

Thicker and more colorful lines, with axes labels and a title.

@interaction[#:eval plot-eval
                    (eval:alts (require plot/pict) (void))
(parameterize [(line-width 2)]
  (plot (list (lines (list #(0 1.5) #(1 3.0)) #:color 'blue)
              (lines (list #(0 3.5) #(1 2.5)) #:color 'orange))
        #:title "Interactive Plot"
        #:x-label "X-axis"
        #:y-label "Y-axis"))]

@section{Getting Data}

I advise using the data-frame package to import your data. It is easy
to work with and does not require dropping into "lower level" operations
to do things like parse strings into numbers.

One way to get data from an external source is to write it to a CSV
file. The file would look like this:

@verbatim{x,y
1,1
2,4
3,9
4,16
5,25}

We put the data into a string so this tutorial does not need any
external files. As a user, you would use a single string that crosses
multiple lines, not using the embedded @tt{\n} used
below. For example, you could copy and paste the comma-separated x and y
coordinates into the string.

The @racket{df-select*} function is used to extract the specified
columns from a data frame and turn them into a list of vectors, one
for each row. This format is suitable for @racket{lines}.

@interaction[#:eval plot-eval-df
                    (eval:alts (require plot data-frame) (void))

(define csv-data "x,y\n1,1\n2,4\n3,9\n4,16\n5,25")
(define csv-port (open-input-string csv-data))
(define df1 (df-read/csv csv-port))
(plot (lines (df-select* df1 "x" "y")))]

