# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Error bars.
#' 
#' This functions adds error bars to a plot or, optionally, plots points and
#' added error bars.
#' 
#' 
#' @aliases errorbars errorbar
#' @param x X axis positions.
#' @param y Y axis positions.
#' @param top Top error bar length.
#' @param bottom Bottom error bar length.
#' @param length The length of the horizontal bars.
#' @param add If TRUE, error bars are added to existing plot.
#' @param ... Additional arguments go to the internal call of arrows() which
#' draws the error bars.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Inspired by a similar function first seen here:
#' 
#' http://monkeysuncle.stanford.edu/?p=485
#' @examples
#' 
#' 
#' ## add bars to existing plots 
#' plot (1:10, 1:10, col = 4, pch = 18)
#' errorbars (1:10, 1:10, .5, col = 4)
#' 
#' ## or create a new plot
#' errorbars (1:10, 1:10, 2, add = FALSE)
#' 
#' 
errorbars = function(x, y, top, bottom = top, length = .2, add = TRUE, ...){
  if (add) arrows(x, y+top, x, y-bottom, angle=90, code=3, length=length, ...)
  if (!add){
    plot (x,y,pch=16, ylim = range(y) + c(-top, bottom))
    arrows(x, y+top, x, y-bottom, angle=90, code=3, length=length, ...)
  }
}


errorbar = function(x, y, top, bottom = top, length = .2, add = TRUE, ...){
  cl = match.call()
  args = sapply (2:length(cl), function(x) cl[[x]])
  names(args) = names(cl)[-1]
  do.call (errorbars, args)
}

