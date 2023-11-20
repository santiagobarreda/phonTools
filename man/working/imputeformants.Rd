\name{imputeformants}
\alias{imputeformants}
\title{
Impute Missing Formant Values
}
\description{
Impute missing formant values using a least-squares approximation.
}
\usage{
imputeformants (ffs, speaker, vowel)
}
\arguments{
  \item{ffs}{A numeric vector containing formant frequency measurements for a single formant. Values should be in Hz, and missing values need to be set to 0.} 
  \item{speaker}{A vector indicating which speaker produced each formant.} 
  \item{vowel}{A vector indicating which vowel category each formant belongs to.} 
}
\details{
This function finds the least-squares approximation for each missing value based on the assumption that each formant for a given vowel differs between-speakers solely on the basis on a speaker-specific multiplicative parameter. This assumption is well supported in the literature (Nearey 1978, Nearey & Assmann 2007, Turner et al. 2009). This parameter would be most closely related to gross speaker vocal-tract length.
}
\value{
A vector containing each original formant frequency and imputed formant frequencies where appropriate.
}
\references{
Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD thesis, Indiana University Linguistics Club. 


Nearey, T. M. & P. F. Assmann. (2007). Pobabilistic 'sliding template' models for indirect vowel normalization. in Experimental Approaches to Phonology, edited by M.J. Sole, P. S., Beddor, and M. Ohala (Oxford University Press, Oxford), pp. 246-269.


Turner, R. E., Walters, T. C., Monaghan, J. J. M., & Patterson, R. D. (2009). A statistical, formant-pattern model for segregating vowel type and vocal-tract length in developmental formant data. The Journal of the Acoustical Society of America, 125(4), 2374. doi:10.1121/1.3079772
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
data (h95)

## Select F2 values from the tenth speaker in H95 data
## set the first 5 values to "missing"
ffs = h95$f2
ffs[h95$speaker == 10][1:5] = 0
speaker = h95$speaker
vowel = h95$vowel

## impute these missing values
imputedf2 = imputeformants (ffs, speaker, vowel)

## resulting in a very close approximation of the original values
plot (imputedf2[h95$speaker == 10], h95$f2[h95$speaker == 10])
abline (0, 1, col = 2)
}





