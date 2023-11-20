# Copyright (c) 2015 Santiago Barreda
# All rights reserved.





#' Standard deviation Ellipse
#' 
#' Draw standard deviation ellipses around a group of observations.
#' 
#' This function may be used in 2 different ways: 1) To draw standard deviation
#' ellipses around a set of observations (if 'means' is not specified) 2) To
#' draw ellipses and circles on plots (if 'means' is specified).
#' 
#' @param points A matrix with two columns in which each row is a different
#' observation from a bivariate normal distribution. Optionally, a 2 by 2
#' covariance matrix may be specified directly in conjunction with the means
#' parameter.
#' @param stdev The number of standard deviations to be enclosed by the
#' ellipse.
#' @param density The spacing between sampling points along the ellipse. A
#' higher number results in a coarser sampling.
#' @param add If TRUE, the ellipse is added to an existing plot. If FALSE, a
#' new plot is created.
#' @param show If FALSE, no ellipse is drawn.
#' @param means A vector of 2 means, one for each dimension. If these are
#' specified, points is assumed to be a covariance matrix rather than a
#' sequence of observations.
#' @param se If TRUE, a standard error (rather than standard deviation) ellipse
#' is plotted.
#' @param ... Additional arguments are passed to the internal call of lines()
#' or plot() as appropriate.
#' @return A matrix is returned where the first column indicate x-axis ellipse
#' coordinates and the second column indicates y-axis ellipse coordinates.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' ## Examples of draws from different bivariate normal distributions
#' ## and standard deviation ellipses drawn to fit them.
#' par (mfrow = c(2,2))
#' draws = rmvtnorm (n = 1000, k = 2, sigma = .3)
#' plot (draws)
#' sdellipse (draws, stdev = 3, lwd = 3, col = 2)
#' 
#' draws = rmvtnorm (n = 1000, k = 2, sigma = -.3)
#' plot (draws)
#' sdellipse (draws, stdev = 3, lwd = 3, col = 2)
#' 
#' draws = rmvtnorm (n = 1000, k = 2, sigma = -.7)
#' plot (draws)
#' sdellipse (draws, stdev = 3, lwd = 3, col = 2)
#' 
#' draws = rmvtnorm (n = 1000, k = 2, sigma = .7)
#' plot (draws)
#' sdellipse (draws, stdev = 3, lwd = 3, col = 2)
#' 
#' ## alternatively, a covariance matrix may be specified directly. 
#' par (mfrow = c(1,1))
#' sdellipse (matrix(c(1,.5,.5,1),2,2), means = c(0,0), 
#' add = FALSE, stdev = 1)
#' sdellipse (matrix(c(1,-.5,-.5,1),2,2), means = c(0,0), 
#' add = TRUE, stdev = 1)
#' 
sdellipse = function (points, stdev = 1.96, density = .01, add = TRUE, 
show = TRUE, means = NULL, se = FALSE, ...){
  if (ncol (points) != 2) stop ('Points input must have exactly two columns.')
  if (!is.null(means) & nrow(points) > 2) stop ('Covariance matrix must be 2 by 2.')
  if (!is.null(means) & length(means) > 2) stop ('Exactly two means must be specified.')

  t = seq (0,2*pi+density,density)  
  x = rbind (cos(t), sin(t))
  if (is.null(means)){
    sigma = var (points)
    if (se) sigma = sigma / nrow(points)
  }
  if (!is.null(means)){
    sigma = points
    if (is.numeric(se)) sigma = sigma / se
  }
  A = eigen(sigma)$vectors %*% (diag(sqrt(eigen(sigma)$values)) * stdev)
  points = t(colMeans(points) + A%*%x)
  if (is.null(means)) points = t(colMeans(points) + A%*%x)
  if (!is.null(means)) points = t(means + A%*%x)
  
  if (add == TRUE & show == TRUE) lines (points, ...)
  if (add == FALSE & show == TRUE) plot (points, type = 'l', ...)
  invisible (points)
}

