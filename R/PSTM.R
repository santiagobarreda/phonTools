# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Probabilistic Sliding Template Model
#' 
#' Classify Vowels Using the Probabilistic Sliding Template Model
#' 
#' The classic log-mean normalization method of Nearey (1978) helps compare the
#' vowels produced by different speakers by controlling for the log-mean
#' formant frequency (FF) produced by a speaker. This approach to normalization
#' assumes that variation in the vowel spaces of speakers of the same dialect
#' is primarily according to a single multiplicative parameter. When this
#' speaker-specific scaling parameter is controlled for, differences in the
#' vowel spaces of different speakers are minimized.
#' 
#' The Probabilistic Sliding Template Model (PSTM) of Nearey and Assmann (2007)
#' attempts to predict perceived vowel quality by 'guessing' an appropriate
#' speaker-specific scaling parameter and normalizing vowels using this
#' estimated parameter. 'Method 6' of the PSTM (described in Nearey & Assmann,
#' 2007) is used to estimate the necessary parameter. After normalization,
#' vowels are classified by comparing them to a provided reference template,
#' which can be created using the createtemplate() function included in this
#' package. Normalized or unnormalized vowels may be classified, as long as the
#' same transformations are performed on the data used to create the template
#' and the data being classified.
#' 
#' If no template is passed, the model identifies vowels relative to the vowel
#' system of Edmonton English speakers. For in-depth details regarding the
#' specifics of this model, please see Nearey & Assmann (2007).
#' 
#' @aliases PSTM mscohere iplot outlier.plot
#' @param ffs A matrix of formant frequencies in which each row represents a
#' single vowel token, and each column represents a formant frequency. At least
#' 3 formants need to be specified for every vowel.
#' @param f0 A vector containing the average f0 measured for each vowel to be
#' classified. The length of this vector must equal the number of rows in ffs.
#' @param template A 'template' object created with the createtemplate()
#' function provided in this package. If no template is specified, vowels are
#' classified relative to vowels from Edmonton English.
#' @param winner If TRUE, only the winner of each classification is returned.
#' If FALSE, information regarding all candidate vowels is returned. See
#' 'value' subsection for details.
#' @return If winner = TRUE:
#' 
#' A dataframe with the following columns:
#' 
#' \item{vowel}{The label for each winning vowel.} \item{psi}{The optimal psi
#' determined for the winning vowel.} \item{postprob}{The posterior probability
#' of observing the winning vowel, given the formants and the psi.}
#' 
#' If winner = FALSE:
#' 
#' A list of dataframes, each of which contains information for every candidate
#' vowel category for each token to be classified. Each dataframe has the
#' following columns:
#' 
#' \item{vowel}{The label for each candidate vowel category.} \item{psi}{The
#' optimal psi determined for each vowel category.} \item{postprob}{The
#' posterior probability of observing each vowel category, given the formants
#' and the psi.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD
#' thesis, Indiana University Linguistics Club.
#' 
#' Nearey, T. M. & P. F. Assmann. (2007). Pobabilistic 'sliding template'
#' models for indirect vowel normalization. in Experimental Approaches to
#' Phonology, edited by M.J. Sole, P. S., Beddor, and M. Ohala (Oxford
#' University Press, Oxford), pp. 246-269.
#' @examples
#' 
#' ## load Peterson & Barney vowel data
#' data (pb52)
#' 
#' ## normalize vowel formant frequencies
#' normdvowels = normalize (pb52[,7:9], pb52$speaker, pb52$vowel)
#' formants = normdvowels[,1:3]
#' vowels = pb52$vowel
#' 
#' ## make a vowel template based on these frequencies
#' template = createtemplate (formants, vowels) 
#' 
#' ## first classify only the first three vowels
#' ffs = pb52[1:3,c(7:9)] 
#' f0 = pb52[1:3,6]
#' 
#' ## outputting only the winners, and then the full posterior probabilities
#' PSTM (ffs, f0, template)
#' PSTM (ffs, f0, template, winner = FALSE)
#' 
#' ## now classify all vowels
#' ## uncomment to run
#' #ffs = pb52[,c(7:9)] 
#' #f0 = pb52[,6]  
#' #winner = PSTM (ffs, f0, template)
#' ## with a good degree of accuracy
#' #table (winner$vowel, pb52$vowel)
#' 
PSTM = function (ffs, f0, template, winner = TRUE){
  ffs = as.matrix(ffs)
  if (length(ffs) == 3 & ncol(ffs) == 1) ffs = t(ffs)
  if (ncol(ffs) < 3) stop ('At least three formants must be provided.')
  n = nrow (ffs)
  f0 = as.matrix(f0)
  if (nrow(ffs) != length(f0)) stop ('Rows in ffs must equal length of f0.')
    
  if (missing(template)){
    covariance = matrix (c(1.471405e-02,1.042306e-03, -1.011399e-03,  1.042306e-03,
    1.005482e-02,  5.882612e-06, -1.011399e-03,  5.882612e-06,  5.674205e-03), 3, 3)
    
    means =  matrix (c(-1.4191406,0.62950438,0.8487829,-0.9816006,0.41787938,
    0.7149399,-0.9211756,0.49334438,0.7382749,-0.7035106,0.32145438,0.7001354,-0.4457406,0.22904938,
    0.6735179,-0.5450806,-0.09682062,0.6557979,-0.6572806,0.01781438,0.6558074,-0.8547056,-0.17574562,
    0.6554719,-0.9187456,-0.09007562,0.6727639,-1.2477756,0.01196438,0.6208954), 10, 3, byrow = TRUE)
    
	rownames (means) = c('i','I','e','E','{','A','V','o','U','u')
	
    template = list (means = means, covariance = covariance)
    class (template) = 'template'
  }
  if (!inherits(template,'template')) stop ('Innapropriate template provided.')

  f0weight=2; covarweight=1; psiprioweight=1;  

  means = as.matrix(template$means)
  if (ncol(ffs) != ncol(means)) stop ('Same number of formants must be provided in ffs and means.')
  covariance = as.matrix(template$covariance)
  if (ncol(covariance) != ncol(means)) stop ('Same number of formants must be provided in covariance and means.')
   
  if (mean(ffs) > 20) ffs = log(ffs)
  if (mean(f0) > 20) f0 = log(f0)
  
  psiPriorMean = 7.2333
  psiPriorVar = (.1284)^2 * 1
  
  f0PsiSlope = 2.14452
  f0PsiIntercept = - 10.3233
  f0VarGivenPsi = (.132703)^2
  
  covariance = solve (covariance * covarweight)
  if (winner == TRUE) output = data.frame()
  if (winner == FALSE) output = list()
  for (i in 1:n){
    outpsi = rep (0, nrow(means))
    dvowels = rep (0, nrow(means))
    ## merged derivative of log-likelihood lines
    ## slope (sd) is constant, only intercept changes
    slope = -sum(covariance) + (-f0PsiSlope/f0VarGivenPsi) + (-1/psiPriorVar)
    baseintercept = f0[i]/f0VarGivenPsi - (f0PsiIntercept/f0VarGivenPsi) + (psiPriorMean/psiPriorVar)
    k = ncol (ffs)  ## this is the multivariate normal density given means and an observation
  
    for (j in 1:nrow(means)){
      intercept = baseintercept + sum ((ffs[i,]-means[j,])%*%covariance) 
      outpsi[j] = -intercept/slope
      normd = ffs[i,] - outpsi[j]
      dvowels[j] = exp( -.5 * ( (normd - means[j,]) %*% covariance %*% (normd - means[j,]) ) )
    }
    dvowels = dvowels / sum (dvowels)
    tmp = data.frame (vowel = rownames(means), psi = outpsi, postprob = dvowels)
    
    if (winner == FALSE) output[[i]] = tmp
    if (winner == TRUE) output = rbind (output, tmp[order(tmp[,3], decreasing = TRUE)[1],])
  }
  if (winner == TRUE) rownames (output) = 1:nrow(output)
  return (output)
}
