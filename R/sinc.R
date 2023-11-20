# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Sinc Function
#' 
#' Sample the sinc function at given points
#' 
#' The formula for the unnormalized sinc function is y = sin(x)/x.
#' 
#' The formula for the normalized sinc function is y = sin(x*pi)/(x*pi).
#' 
#' @param x A vector of x-axis points at which the sinc function will be
#' sampled.
#' @param normalized If TRUE, a normalized sinc function is returned.
#' @return A vector containing y-axis values for each specified x-axis value is
#' returned.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/Sinc_function
#' 
#' Lyons, R. G. (2004). Understanding Digital Signal Processing (2nd ed.).
#' Prentice Hall.
#' @examples
#' 
#' x = seq(-20,20,.1)
#' ## generate both forms of the sinc function between -20 and 20
#' y1 = sinc (x)
#' y2 = sinc (x, normalized = TRUE)
#' ## the unnormalized sinc function has zero crossing at 
#' ## integer multiples of pi
#' plot (x,y1, type = 'b')
#' ## the normalized sinc function has zero crossing at integers
#' lines (x,y2, type = 'b', col = 4)
#' abline (h = 0)
#' 
sinc = function (x, normalized = FALSE){ 
  if (normalized == FALSE) output = sin(x)/x  
  if (normalized == TRUE) output = sin(x*pi)/(x*pi)  
  output[x==0] = 1
  output
}


