#' Combinations and Permutations
#' 
#' Calculate the number of combinations or permutations for a number of
#' objects.
#' 
#' 
#' @export
#' @importFrom grDevices colorRampPalette dev.cur dev.list
#' @importFrom graphics axis boxplot hist lines plot points polygon
#' segments text title .filled.contour Axis abline arrows box identify
#' image layout layout.show locator par plot.new plot.window 
#' @importFrom stats aggregate cov fft filter frequency lm mahalanobis pf
#' prcomp printCoefmat pt rnorm sd ts var
#' @importFrom utils flush.console head read.table tail
#' @param objects The number of different kinds of objects available for
#' selection.
#' @param choose The number of objects selected at a given time.
#' @param order If TRUE, the order of the objects matters, for example aba !=
#' aab.
#' @param repetition If TRUE, a sequence such as bbb is permissible.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples 
#' # combinations, no repetition
#' combocalc (10, 4, order = FALSE, repetition = FALSE)
#' # combinations, with repetition
#' combocalc (10, 4, order = FALSE, repetition = TRUE)
#' # permutations, no repetition
#' combocalc (10, 4, order = TRUE, repetition = FALSE)
#' # permutations, with repetition
#' combocalc (10, 4, order = TRUE, repetition = TRUE)
#' 
 

combocalc = function (objects, choose, order = FALSE, repetition = TRUE){
  if (length(objects) > 1) stop ('Incorrect objects input.')
  if (!is.numeric (objects)) stop ('Incorrect objects input.')
  if (length(choose) > 1) stop ('Incorrect order input.')
  if (!is.numeric (choose)) stop ('Incorrect order input.')
  
  if (order == TRUE){
   if (repetition == TRUE) combos = objects^choose
   if (repetition == FALSE) combos = gamma (objects+1) / gamma (objects-choose+1)
  }
  if (order == FALSE){
    if (repetition == TRUE) combos = gamma (objects+choose) / (gamma(choose+1)*gamma(objects))
    if (repetition == FALSE) combos = gamma (objects+1) / (gamma(objects-choose+1)*gamma(choose+1))
  }
  combos
}
