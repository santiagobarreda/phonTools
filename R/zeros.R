
#' Zero Vector/Matrix
#' 
#' Returns a vector or 2D matrix of zeros of the same shape as the input.
#' 
#' If a scalar value is given, a vector of that many zeros is returned. If the
#' y parameter is also specified, a matrix is returned with x rows and y
#' columns.
#' 
#' If a vector is given, a vector of zeros of the same length is returned.
#' 
#' If a 2D matrix or dataframe is given, a matrix of zeros with the name number
#' of rows and columns is returned.
#' 
#' @param x A scalar, vector, or two dimensional matrix.
#' @param y An optional parameter to create a 2D matrix.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' ## A vector 
#' zeros (5)
#' ## A matrix
#' zeros (5, 3)
#' 
#' ## Copy an existing vector
#' zeros (1:10)
#' 
#' ## Copy an existing matrix
#' samplematrix = 1:10 %*% t(2:11)
#' zeros (samplematrix)
#' 
#' 
zeros = function (x,y=0){
  if (missing(x)) return (0)
  if (length(x) == 1 & y == 0) return (rep (0, x))
  if (length(x) == 1 & y > 0)  return (matrix (0, x,y))

  nr = nrow(x)
  nc = ncol(x)

  if (is.null(nr) | is.null(nr)){
    output = rep(0, length(x))
    return (output)
  }
  output = matrix (0, nr ,nc)
  return (output)
}
