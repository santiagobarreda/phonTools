# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Interpolation
#' 
#' Piece-wise cubic or linear spline interpolation.
#' 
#' By default, this function performs a 'natural' cubic spline interpolation
#' between the points provided by the user. Optionally, a linear interpolation
#' between the points may be carried out.
#' 
#' @param y A vector of 'knots', between which the function will interpolate
#' points.
#' @param x The 'x' coordinates corresponding to each knot. If not specified,
#' the knots are assumed to be equally spaced.
#' @param steps The number of interpolating steps between each knot. Increasing
#' this number will result in a smoother interpolation. If the knots are not
#' equally spaced along the x-axis, the interpolated points will not be equally
#' spaced across the entire curve.
#' @param increment If this is greater than 0, interpolated points are
#' separated along the x-axis by this value. Note that if the knot locations
#' are not multiples of this increment, there will be irregularities in the
#' spacing of the interpolated points.
#' @param show If TRUE, the result of the interpolation is shown in a plot.
#' @param type If 'cubic', a natural cubic spline interpolation is performed.
#' If 'linear', a linear interpolation is performed.
#' @param ... Additional arguments are passed to the internal call of plot() if
#' show = TRUE.
#' @return A dataframe with columns corresponding to the x and y dimensions of
#' the interpolated points is returned.
#' 
#' \item{x}{The x-axis coordinates of the interpolated points.} \item{y}{The
#' y-axis coordinates of the interpolated points.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/Spline_interpolation
#' @examples
#' 
#' ## generate ten random points
#' y = rnorm (10, 0, 5)
#' interpolate (y, show = TRUE)  ## plot a cubic interpolation
#' linear = interpolate (y, type = 'linear')   
#' ## and compare to a linear interpolation
#' lines (linear, col = 2) 
#' 
interpolate = function (y, x = 1:length(y), steps = 20, increment = -1, show = FALSE, type = 'cubic', ...){
  if (length (y) < 3) stop ('Interpolation requires at least three points.')
  if (!is.numeric (y) | !is.numeric (x)) stop ('Non-numeric arguments provided')
  if (type == 'cubic'){
    n = length(y)
    h = x[2:n] - x[1:(n - 1)]
    A = matrix(0, n, n)
    A[1, ] = c(1, rep(0, n - 1))
    A[n, ] = c(rep(0, n - 1), 1)
    for (i in 2:(n - 1)) {
      A[i, i - 1] = h[i - 1]
      A[i, i] = 2 * (h[i - 1] + h[i])
      A[i, i + 1] = h[i]
    }
    r = matrix(0, n, 1)
    for (i in 2:(n - 1)) r[i] = 6 * (((y[i + 1] - y[i])/h[i]) - 
      ((y[i] - y[i - 1])/h[i - 1]))
    m = c(0, lm(r ~ A)$coefficients[3:n], 0)
    yinterp = NULL
    xinterp = NULL
	
    for (i in 1:(length(x) - 1)) {
      a = y[i]
      b = ((y[i + 1] - y[i])/h[i]) - ((h[i]/2) * m[i]) - (h[i]/6) * (m[i + 1] - m[i])
      c = m[i]/2
      d = (m[i + 1] - m[i])/(6 * h[i])
      if (increment <= 0) xx = seq(x[i], x[i + 1], length.out = steps)
      if (increment > 0) xx = seq(x[i], x[i + 1], by = increment)
      yy = a + b * (xx - x[i]) + c * ((xx - x[i])^2) + d * ((xx - x[i])^3)
      xinterp = c(xinterp, xx)
      yinterp = c(yinterp, yy)
    }
  }
  if (type == 'linear'){
    n = length (y) - 1
    xinterp = NULL
    yinterp = NULL
    for (i in 1:n){
      if (increment <= 0){
        xinterp = c(xinterp, seq (x[i], x[i+1], length.out = steps))
        yinterp = c(yinterp, seq (y[i], y[i+1], length.out = steps))
      }
      if (increment > 0){
        xinterp = c(xinterp, seq (x[i], x[i+1], by = increment))
        yinterp = c(yinterp, seq (y[i], y[i+1], length.out = length(seq (x[i], x[i+1], by = increment))))
        
      }
    }
  }
  if (show == TRUE) {
    plot(xinterp, yinterp, type = "l")
    points(x, y)
  }
  output = data.frame(x = xinterp, y = yinterp)
  output = output[output[,1] != c(output[-1,1], 'q'),]
  rownames(output) = 1:nrow(output)
  invisible (output)
}
