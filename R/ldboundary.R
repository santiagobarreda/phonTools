
#' Linear Discriminant Boundary
#' 
#' Given two mean vectors and a covariance matrix (and optional prior
#' probabilities), this function will return the slope and intercept of the
#' boundary line between the two categories.
#' 
#' 
#' @export
#' @param mean1 A mean vector for category 1, must contain 2 elements.
#' @param mean2 A mean vector for category 2, must contain 2 elements.
#' @param covariance A 2x2 covariance matrix for both distributions.
#' @param prior1 The prior probability of category 1.
#' @param prior2 The prior probability of category 2.
#' @param add If TRUE, the boundary line is added top the plot.
#' @param ... Additional parameters are passed to the internal call of the line
#' plotting function, in the event that add = T.
#' @return The slope and intercept of the boundary line are returned.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/Linear_discriminant_analysis
#' https://onlinecourses.science.psu.edu/stat557/book/export/html/35
#' @examples
#' 
#' 
#' ## create two groups with the same covariance patterns
#' group1 = rmvtnorm (200, means= c(0,0), k=2, sigma = -.4)
#' group2 = rmvtnorm (200, means= c(3,3), k=2, sigma = -.4)
#' covariance = (var (group1) + var (group2)) / 2
#' 
#' ## plot groups and boundary line between categories.
#' plot (group1, col = 2, pch = 16, ylim = c(-2,5), xlim = c(-2,5))
#' points (group2, col = 4, pch = 16)
#' ldboundary (c(0,0), c(3,3), covariance, add = TRUE)
#' 
#' 
ldboundary = function (mean1, mean2, covariance, prior1 = .5, prior2 = .5, add = F, ...){
  w = solve (covariance) %*% (mean1-mean2)
  x0 = 1/2 * (mean1+mean2) - (log(prior1/prior2) /
       t(mean1-mean2) %*% solve(covariance) %*% (mean1-mean2)) %*%
       (mean1 - mean2)
  x0 = matrix (x0,2,1)
  coeffs = c(intercept = -(t(w) %*% x0)/-w[2],slope = -w[1]/w[2])
  if (add) abline (coeffs, ...)
  coeffs
}

