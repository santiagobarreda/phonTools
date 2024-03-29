% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vplot.R
\name{vplot}
\alias{vplot}
\alias{IPA}
\title{Plot Vowels}
\usage{
vplot(
  x,
  y,
  labels = NULL,
  colors = NULL,
  points = NULL,
  meansonly = FALSE,
  ellipsesd = 0,
  add = FALSE,
  alternateaxes = FALSE,
  xsampa = FALSE,
  logaxes = FALSE,
  ...
)
}
\arguments{
\item{x}{A numeric vector indicating formant frequencies to be plotted on
the x axis.}

\item{y}{A numeric vector indicating formant frequencies to be plotted on
the y axis.}

\item{labels}{A vector with labels for vowels. Must be provided for any
category-dependent differences in plotting. If x-sampa labels are given IPA
symbols may be plotted.}

\item{colors}{Colors to use for different categories. If specified this
overrides automatic colors. It cycles through the list given if number of
colors are less than number of categories.}

\item{points}{Kinds of points to use determined by 'pch' value. If specified
it overrides text labels. IPA symbols may be plotted by finding appropriate
values using the pickIPA() function included in this package.}

\item{meansonly}{If TRUE, only category means are plotted (labels must be
provided).}

\item{ellipsesd}{If a number greater than zero is given, ellipses are drawn
enclosing this many standard deviations (one per category as indicated by
label vector).}

\item{add}{If TRUE, vowels are plotted on existing figure. If FALSE, a new
one is created.}

\item{alternateaxes}{If TRUE, the origin in the top right corner of the
plot, resulting in a configuration like the IPA vowel quadrilateral if F1
and F2 are provided. By default the origin in the bottom left corner.}

\item{xsampa}{If TRUE, the labels vector given to the function is assumed to
be specified in x-sampa and IPA symbols are used to plot using the
xsampatoIPA() function included in this package. If this is set to TRUE and
the 'labels' input is not in x-sampa, the symbols will be wrong.}

\item{logaxes}{Linear axes are used by default, for log axes set to TRUE.}

\item{\dots}{Additional arguments are passed to the internal call of 'plot'.}
}
\description{
A flexible function that can create a wide variety of vowel plots (including
IPA symbols).
}
\details{
*** This function replaces the older vowelplot() function, which has been
deprecated. ***

A flexible vowel plotting function, including functionality to easily
generate vowel plots using IPA symbols. This relies on category labels being
specified in x-sampa (the required plotting values for IPA symbols may be
selected using the pickIPA() function included in this package).

Default parameter values are set for the plot, but these may all be
overridden using the standard plotting parameters.

There may be issues when exporting figures to PDF using IPA font. Exporting
plots directly as images works 'out of the box'.
}
\examples{

## A few examples of some vowel plots. 

## load the Peterson and Barney data
data (pb52)
pb52 = pb52[pb52$type=='m',]  ## use only the males

par (mfrow = c(3,2), mar = c(4.2,4.2,1,1))

# standard layout with linear axes
vplot (pb52$f1, pb52$f2, pb52$vowel, xsampa = TRUE)

# alternate layout with log axes
vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = TRUE, 
           alternateaxes = TRUE, xsampa = TRUE)

# category means only 
vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = TRUE, 
           meansonly = TRUE, xsampa = TRUE, cex = 3)

# category means only with standard deviation ellipses
vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = FALSE, 
       meansonly = TRUE, ellipsesd = 2, xsampa = TRUE)

# same as above, with alternate axes
vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = TRUE, 
       meansonly = TRUE, ellipsesd = 2, xsampa = TRUE, 
	   alternateaxes = TRUE)

# individual points with standard deviation ellipses
# and alternate axes
vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = TRUE, 
	   meansonly = FALSE, ellipsesd = 2, xsampa = TRUE, 
	   alternateaxes = TRUE)


}
\references{
http://en.wikipedia.org/wiki/X-SAMPA
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
