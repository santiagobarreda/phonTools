# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Rotate
#' 
#' Rotate 2D and complex-valued observations. The output is of the same kind as
#' the input.
#' 
#' 
#' @param xy Either, a vector of complex-valued observations, or a matrix with
#' 2 columns and any number of rows.
#' @param angle The desired angle of rotation, in radians.
#' @param degrees If angle of rotation is specified in degrees instead of
#' radians, this should be set to TRUE.
#' @param origin If TRUE, points are rotated about the origin. If FALSE, points
#' are rotated 'in place' about the mean for each dimension (i.e. the central
#' location of the distribution).
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' ## rotate points in a 2D space 
#' mat = cbind (1:100, 100:1)
#' rotate (mat, pi/2)
#' 
#' ## rotate complex-valued numbers
#' complx = complex (real = mat[,1], imaginary = mat[,2])
#' rotate (complx, pi/2)
#' 
#' 
rotate = function (xy, angle, degrees = FALSE, origin = TRUE){
  complx = FALSE
  if (degrees) angle = angle * pi/180
  if (is.complex(xy)) {
    xy = cbind(Re(xy), Im(xy))
    complx = TRUE
  }
  if (ncol(xy) != 2) stop("Input must have two columns (2-dimensional).")

  if (!origin){
    mus = colMeans (xy)
    xy = xy - matrix (mus,nrow(xy),2,byrow = T)
  }
  
  rotmat = matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
  output = as.matrix(xy) %*% rotmat
  
  if (!origin) output = output + matrix (mus,nrow(output),2,byrow = T)
  
  if (complx) output = complex(real = output[, 1], imaginary = output[,2])
  output
}

