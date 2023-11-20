# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Hotelling's T2 Test
#' 
#' Hotelling's T2 test for one and two samples.
#' 
#' If a single matrix is provided, this function tests the alternative
#' hypothesis that all column means are not equal to zero. If a second matrix
#' is provided, the alternative hypothesis to be tested is that the group means
#' are not all equal. The statistic is tested using an F-distribution which
#' assumes that the matrices represent (roughly) multivariate normal variables.
#' 
#' This function is only designed for multivariate tests of location. If a
#' univariate test is desired, please use a t-test.
#' 
#' @aliases hotelling.test print.hotelling.test
#' @param matrix1 A numeric matrix or dataframe in which each row represents an
#' observation of a multivariate random variable, and each column represents a
#' dimension of that variable.
#' @param matrix2 An optional second numeric matrix or dataframe of the same
#' column rank as 'matrix1'.
#' @return An object of class 'Hotelling.test', a list containing the elements:
#' 
#' \item{f.value}{The value of the test statistic.} \item{df1}{The numerator
#' degrees of freedom for the F statistic.} \item{df2}{The denominator degrees
#' of freedom for the F statistic} \item{p.value}{The p-value for the test.}
#' \item{samples}{The number of independent samples involved in the test.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Hotelling, H. (1931). The generalization of Student's ratio.
#' Annals of Mathematical Statistics 2 (3): 360-378.
#' 
#' http://en.wikipedia.org/wiki/Hotelling's_T-squared_distribution
#' @examples
#' 
#' ## load Peterson & Barney data
#' data (pb52)
#' 
#' ## separate the Peterson & Barney vowels by speaker
#' ## gender and age (child vs. adult)
#' men = pb52[pb52$sex == 'm' & pb52$type == 'm',]
#' women = pb52[pb52$sex == 'f' & pb52$type == 'w',]
#' boys = pb52[pb52$sex == 'm' & pb52$type == 'c',]
#' girls = pb52[pb52$sex == 'f' & pb52$type == 'c',]
#' 
#' ## fit 4 separate models which predict F1 frequency 
#' ## on the basis of vowel category. 
#' men = rcr (f1 ~ vowel, men$speaker, men)
#' women = rcr (f1 ~ vowel, women$speaker, women)
#' boys = rcr (f1 ~ vowel, boys$speaker, boys)
#' girls = rcr (f1 ~ vowel, girls$speaker, girls)
#' 
#' ## A Hotelling T2 test indicates that there are 
#' ## significant differences in F1 frequency 
#' ## based on vowel category between males and females
#' hotelling.test (men$coefficients, women$coefficients)
#' 
#' ## but no significant differences based on the same 
#' ## criteria between boys and girls.
#' hotelling.test (boys$coefficients, girls$coefficients)
#' 
hotelling.test <-
function (matrix1, matrix2 = NULL){

  if (is.null(ncol (matrix1))) return (cat ('Error: Univariate variable provided. Use a t-test.\n\n'))
  
  if (is.null(matrix2)){
    samples = 1
    mus1 = colMeans (matrix1); n1 = nrow (matrix1); p = ncol (matrix1)
    df1 = p; df2 = n1 - p; covar = solve (var (matrix1)/n1)
    f.value = t(mus1) %*% covar %*% (mus1) * ((n1 - p) / (p *(n1 -1)))
  }
  if (!is.null(matrix2)){
    samples = 2
    if (is.null(ncol (matrix2))) return (cat ('Error: Univariate variable provided. Use a t-test.\n\n'))
    if (ncol(matrix1) != ncol (matrix2)) return (cat ('Error: Variable dimensions do not match.\n\n'))

    mus1 = colMeans (matrix1); n1 = nrow (matrix1); p = ncol (matrix1); df1 = p;
    mus2 = colMeans (matrix2)
    n2 = nrow (matrix2)
    df2 = n1 + n2 - p - 1

    covar =  solve (((var(matrix1)*(n1-1) + var(matrix2)*(n2-1)) / (n1+n2-2)))
	
    f.value = t(mus1 - mus2) %*% covar %*% (mus1 - mus2) * ((n1*n2) / (n1+n2)) 
    f.value = f.value * ((n1 + n2 - p - 1) / (p*(n1 + n2 - 2)))  
  }
  print (paste(f.value,df1,df2))
  p.value = 1 - pf (f.value, df1, df2)

  output = list (f.value = f.value, df1 = df1, df2 = df2, p.value = p.value, samples = samples)
  class(output) = 'hotelling.test'
  output
}
