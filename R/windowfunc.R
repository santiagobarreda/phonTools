# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Window Function
#' 
#' Generates a window function of a given type and length.
#' 
#' A window function is generated, of the type and length specified by the
#' user. This is returned as a numeric vector.
#' 
#' @param npoints The desired window length, in points. If a vector is given,
#' the window will have the same length as the vector.
#' @param type A string indicating the type of window desired. For the sake of
#' simplicity, all window names are in lowercase. Supported types are:
#' rectangular, hann, hamming, cosine, bartlett, gausian, and kaiser.
#' @param parameter The parameter necessary to generate the window, if
#' appropriate. At the moment, the only windows that require parameters are the
#' Kaiser and Gaussian windows. By default, these are set to 2 for kaiser and
#' 0.4 for gaussian windows.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/Window_function
#' @examples
#' 
#' par (mfrow = c(1,4))
#' plot (windowfunc (100, 'hann'))
#' plot (windowfunc (100, 'bartlett'))
#' plot (windowfunc (100, 'kaiser', parameter = 2))
#' plot (windowfunc (100, 'kaiser', parameter = 6))
#' 
windowfunc = function (npoints, type = 'hann', parameter = -1){
  if (length (npoints) > 1) npoints = length (npoints) 

  if (!is.numeric (npoints)) stop ('Invalid number of points specified.')
  npoints = round (npoints)
  N = npoints
  n = 0:(N-1)
  output = NULL
  
  if (type == 'rectangular') output = rep (1, N)
  if (type == 'blackman') output = (7938/18608) - (9240/18608)*cos((2*pi*n)/(N-1)) + (1430/18608)*cos((4*pi*n)/(N-1))
  if (type == 'hann' | type == 'hanning') output = 0.5 * (1 - cos ((2*pi*n)/(N-1)))
  if (type == 'hamming') output = 0.54 - 0.46 * cos ((2*pi*n)/(N-1))
  if (type == 'cosine' | type == 'sine') output = sin ((n*pi) / (N-1))
  if (type == 'bartlett') output = (2 / (N-1)) * ((N-1)/2 - abs(n - (N-1)/2))
  if (type == 'gaussian'){
    if (parameter == -1) parameter = 0.4
    output = exp (-.5 * ((n - (N-1)/2) / (parameter * (N-1)/ 2))^2)
  }
  if (type == 'kaiser'){
    if (parameter == -1) parameter = 2
    output = besselI (parameter*pi * sqrt (1 - (2*(n)/(N-1) -1)^2), 0) / besselI(parameter*pi, 0)
  }
  if (type =='') stop ('No window type provided.')
  if (is.null(output)) stop ('Invalid window type provided.')
  return (output)
}

