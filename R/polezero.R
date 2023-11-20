# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Pole-zero Plots
#' 
#' Generate a Pole-zero plot from filter coefficients.
#' 
#' This function plots filter poles (x) and zeros (o) based on the given
#' coefficients.
#' 
#' @param b The filter moving-average (MA) coefficients.
#' @param a The filter autoregressive (AR) coefficients.
#' @param ... Additional parameters are passed to the internal call of plot().
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/Pole%E2%80%93zero_plot
#' @examples
#' 
#' ## example of a typical single-zero preemphasis filter
#' a = 1
#' b = c(1, -.94)
#' polezero (b, a)
#' 
#' #example of a complex-pole formant-style filter
#' a = c(1, -.3, .2)
#' b = c(1)
#' polezero (b, a)
#' 
polezero = function (b, a, ...){
  if (!is.numeric(a)) stop ('Inappropriate feedback coefficients.')
  if (!is.numeric(a)) stop ('Inappropriate feedforward coefficients.')

  aroots = polyroot (rev(a))
  broots = polyroot (rev(b))
  if (length(a) == 1) aroots = complex (real = 0, imaginary = 0)
  if (length(b) == 1) broots = complex (real = 0, imaginary = 0)

  xrange = yrange = range (-1.1,1.1)

  realmax = max(abs (Re(aroots)), abs (Re(broots)))
  imagmax = max(abs (Im(aroots)), abs (Im(broots)))
 
  if (realmax > 1) xrange = c(-realmax, realmax)
  if (imagmax > 1) yrange = c(-imagmax, imagmax)
 
  plot (aroots, xlim = xrange, ylim = yrange, pch = 4, lwd = 2, 
        xlab = 'Real', ylab = 'Imaginary', cex = 1.5)
  points (broots, lwd = 2, cex = 1.75)

  sdellipse (means = c(0,0), points = matrix (c(1,0,0,1),2,2), stdev = 1, density = .01)
  abline (h = 0, v = 0, lty = 'dotted')
}

