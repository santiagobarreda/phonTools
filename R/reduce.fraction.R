# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Reduce Fractions
#' 
#' Reduce fractions to lowest terms using Euclid's Algorithm.
#' 
#' 
#' @param ratio A vector with two integers. The first element is the numerator
#' of a ratio, and the second element is the denominator.
#' @return A vector containing the elements of the reduced fraction. The first
#' element is the numerator of a ratio, and the second element is the
#' denominator.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' ## an easy one
#' reduce.fraction (c(100, 200))
#' 
#' ## irreducible
#' reduce.fraction (c(140, 201))
#' 
#' ## a hard one
#' reduce.fraction (c(140, 203))
#' 
reduce.fraction = function (ratio){
  if (length (ratio) != 2) stop ('Ratio must contain two numbers.')
  if (ratio[1] %% 1 > 0 | ratio[2] %% 1 > 0) stop ('Elements of ratio must be whole numbers.')

  numerator = ratio[1]
  denomenator = ratio[2]

  remainder = -1
  while (remainder != 0){
    remainder = numerator %% denomenator    
    numerator  = denomenator
    if (remainder != 0) denomenator = remainder
  }
  ratio = ratio / denomenator
  return (ratio)
}

