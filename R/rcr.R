# Copyright (c) 2015 Santiago Barreda
# All rights reserved.





#' Random Coefficients Regression
#' 
#' Carry out a random coefficients regression (rcr) using repeated calls to
#' glm, individually for the data from each participant/data cluster.
#' 
#' This function fits a model to the data from each participant individually
#' using repeated calls to glm(). Significance testing is then carried out on
#' the coefficients fit for each participant using the methods established in
#' Gumpertz & Pantula (1989) and Lorch & Myers (1990).
#' 
#' In perceptual experiments there is frequently a high number of data points
#' collected from each participant, and the data collected from each
#' participant is balanced by design. In these situations rcr performs
#' comparably to mixed-effects models. In the event that only a small number of
#' observations are made from each listener, or the data is not balanced, rcr
#' may not be appropriate.
#' 
#' A call to summary() on an rcr object performs a one-sample t-test on each
#' coefficient to test whether it is significantly different from zero.
#' 
#' A call to anova() on an rcr object performs a one-sample t-test in the case
#' of single coefficients, and a one-sample Hotelling T2 test in the event that
#' multiple coefficients are associated with a single factor, to test that they
#' are not all equal to zero.
#' 
#' A call to plot() on an rcr object displays the density corresponding to the
#' distribution of all fitted coefficients. These are compared to a normal
#' distribution with the same mean and standard deviation.
#' 
#' @aliases rcr print.rcr print.summary.rcr summary.rcr anova.rcr
#' print.anova.rcr plot.rcr
#' @param formula A symbolic description of the model to be fitted.
#' @param participants A vector indicating which row in the dataframe belongs
#' to which participant. Length must equal the number of rows in the dataframe.
#' @param dataframe The dataframe containing the data for the model.
#' @param ... Additional arguments to be passed to the internal glm() function
#' call. For example, family, if not 'gaussian', should be specified.
#' @return An object of class 'rcr', a list containing the elements:
#' 
#' \item{formula}{The formula used to call and create the rcr object.}
#' \item{call}{The exact call used to create the rcr object.}
#' \item{participants}{A vector indicating the labels used to identify each
#' individual participant as indicated by 'participants'.} \item{factors}{A
#' vector indicating the grouping of the explanatory variables.}
#' \item{factor.names}{A vector containing the names of each group of
#' coefficients.} \item{coefficients}{A dataframe containing the coefficients
#' fit individually for each participant.} \item{coefficient.means}{The mean
#' value of each coefficient across all participants.}
#' \item{coefficient.names}{The name of each individual coefficient.}
#' \item{varExp}{The percent of variance or deviance explained by the model for
#' each participant.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Gumpertz, M., & Pantula, S. G. (1989). A Simple Approach to
#' Inference in Random Coefficient Models. The American Statistician, 43(4),
#' 203-210.
#' 
#' Lorch, T. F. & Myers, J. L. (1990). Regression analyses of repeated measures
#' data in cognitive research. J. Exp. Psychol. Learn. Mem. Cogn. 16: 149-157.
#' @examples
#' 
#' data (pb52)
#' 
#' ## runs an rcr model on the Peterson & Barney (1952) vowels to test 
#' ## for the predictive value of the speaker's f0 and F3
#' rcr.model = rcr (f1 ~ f0 * f3, pb52$speaker, pb52)
#' rcr.model
#' 
#' ## test for the significance of each individual coefficient 
#' summary (rcr.model)
#' 
#' ## a similar analysis can be run using vowel category as the predictor
#' rcr.model = rcr (f1 ~ vowel, pb52$speaker, pb52)
#' rcr.model
#' 
#' ## here, summary() tests each coefficient individually
#' summary (rcr.model)
#' 
#' ## while anova() tests associated coefficients together
#' anova (rcr.model)
#' 
rcr = function (formula, participants, dataframe, ...){
  if (length (participants) != nrow(dataframe)) return (cat("Error: Dataframe rows and participant vector length do not equal.\n\n"))
  parts = as.factor(levels (as.factor (participants)))
  nparts = length (parts)
  
  coefficients = matrix (0, nparts, ncol (model.matrix (formula, data = dataframe)))
  varExp = NULL  
  
  for (i in 1:nparts){
    temp = dataframe[participants == parts[i],]
    mod =  glm (formula, data = temp, ...)
    varExp = c(varExp, (mod$null.deviance - mod$deviance) / mod$null.deviance)
    coefficients[i,] = mod$coefficients
  }
  
  factor.names = attr(model.frame (formula, data = dataframe), "terms") 
  factor.names = c('(Intercept)', attr (factor.names, 'term.labels'))

  factors = attr(model.matrix (formula, data = dataframe), "assign")
  coefficients = data.frame (coefficients)
  coefficient.names = names(mod$coefficients)
  colnames(coefficients) = coefficient.names 
  coefficient.means = as.numeric (colMeans (coefficients))
  names (coefficient.means) = coefficient.names
  colnames(coefficients) = names(mod$coefficients)

  output =  (list (formula = formula, call = match.call(), participants = parts, 
  factors = as.factor(factors), factor.names = factor.names, coefficients = coefficients, 
  coefficient.means = coefficient.means, coefficient.names = coefficient.names, varExp = varExp))
  
  class (output) = 'rcr'
  output
}
