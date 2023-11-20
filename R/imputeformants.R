# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Impute Missing Formant Values
#' 
#' Impute missing formant values using a least-squares approximation.
#' 
#' This function finds the least-squares approximation for each missing value
#' based on the assumption that each formant for a given vowel differs
#' between-speakers solely on the basis on a speaker-specific multiplicative
#' parameter. This assumption is well supported in the literature (Nearey 1978,
#' Nearey & Assmann 2007, Turner et al. 2009). This parameter would be most
#' closely related to gross speaker vocal-tract length.
#' 
#' @param ffs A numeric vector containing formant frequency measurements for a
#' single formant. Values should be in Hz, and missing values need to be set to
#' 0.
#' @param speaker A vector indicating which speaker produced each formant.
#' @param vowel A vector indicating which vowel category each formant belongs
#' to.
#' @return A vector containing each original formant frequency and imputed
#' formant frequencies where appropriate.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD
#' thesis, Indiana University Linguistics Club.
#' 
#' Nearey, T. M. & P. F. Assmann. (2007). Pobabilistic 'sliding template'
#' models for indirect vowel normalization. in Experimental Approaches to
#' Phonology, edited by M.J. Sole, P. S., Beddor, and M. Ohala (Oxford
#' University Press, Oxford), pp. 246-269.
#' 
#' Turner, R. E., Walters, T. C., Monaghan, J. J. M., & Patterson, R. D.
#' (2009). A statistical, formant-pattern model for segregating vowel type and
#' vocal-tract length in developmental formant data. The Journal of the
#' Acoustical Society of America, 125(4), 2374. doi:10.1121/1.3079772
#' @examples
#' 
#' data (h95)
#' 
#' ## Select F2 values from the tenth speaker in H95 data
#' ## set the first 5 values to "missing"
#' ffs = h95$f2
#' ffs[h95$speaker == 10][1:5] = 0
#' speaker = h95$speaker
#' vowel = h95$vowel
#' 
#' ## impute these missing values
#' imputedf2 = imputeformants (ffs, speaker, vowel)
#' 
#' ## resulting in a very close approximation of the original values
#' plot (imputedf2[h95$speaker == 10], h95$f2[h95$speaker == 10])
#' abline (0, 1, col = 2)
#' 
imputeformants = function (ffs, speaker, vowel){
  if (!is.numeric (ffs)) stop ('Non-numeric formant frquency values provided.') 
  
  vowel = as.factor (vowel)
  speaker = as.factor (speaker)
  lvs = levels (vowel)
  lsp = levels (speaker)

  nvs = length (lvs)
  nsp = length (lsp)

  if (max(ffs) > 20) ffs = log (ffs)
  omit = !(ffs == -Inf)
  mod = lm (ffs[omit] ~ vowel[omit] + speaker[omit])
  cffs = mod$coefficients
  
  vcffs = as.numeric (c(cffs[1], cffs[1]+cffs[2:nvs]))
  pcffs = c(0,cffs[(nvs+1):(nvs+nsp-1)])

  ffs = exp(ffs) 
  for (i in 1:length (ffs))
    if (ffs[i] == 0) 
      ffs[i] = exp (vcffs[lvs == vowel[i]] + pcffs[lsp == speaker[i]])
  
  return (round(ffs))
}

