
#' Number of Unique Elements
#' 
#' Find the number of unique elements in a vector.
#' 
#' A simple function that converts a vector to a factor, and finds the number
#' of levels. This provides the number of unique elements in a vector,
#' something I find I frequently need.
#' 
#' @export
#' @param vector The vector of interest.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' data (pb52)
#' ## the number of unique vowel categories.
#' ntypes (pb52$vowel)
#' 
ntypes = function (vector) length (levels(as.factor(as.character(vector))))
