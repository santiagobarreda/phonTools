
#' Random multivariate normal variables
#' 
#' Draw vectors from a multivariate normal distribution.
#' 
#' If means and sigma are not specified, a standard normal distribution is
#' assumed along every dimensions, and dimensions are assumed to be
#' uncorrelated. If the number of dimensions is not specified, a bivariate
#' distribution is assumed.
#' 
#' @export
#' @param n The number of vectors to be drawn.
#' @param k The dimension of the vectors to be drawn.
#' @param means A vector of means, one for each dimension.
#' @param sigma The covariance matrix of the distribution. If a number between
#' 0 and 1 is provided, this is assumed to be the correlation parameter for a
#' bivariate standard normal distribution.
#' @return A matrix with rows equal to n and columns equal to k, where each row
#' indicates a single observation, and each column represents a different
#' dimension.
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
rmvtnorm = function (n = 1, k = 2, means = rep(0, k), sigma = diag(k)){
    if (length(sigma) == 1) if (sigma >= -1 & sigma <= 1) 
            sigma = matrix(c(1, sigma, sigma, 1), 2, 2)
    else stop("Correlation magnitude must be less than or equal to 1.")
    
    if (!identical(sigma, t(sigma))) 
        stop("Inappropriate covariance matrix specified.")
    
    if (nrow(sigma) != k | ncol(sigma) != k | length(means) != k) 
        stop("Incorrect covariance matrix dimensions.")
    
    eig = eigen(sigma, symmetric = TRUE)
    if (sum(eig$values < 0) > 0) 
        stop("Inappropriate covariance matrix specified.")
    
    A = eig$vectors %*% diag(sqrt(eig$values))
    x = matrix(rnorm(n * length(means), 0, 1), k, n)
    output = t(means + A %*% x)
    return(output)
}
