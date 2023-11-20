\name{PSTM}
\alias{PSTM}
\alias{mscohere}
\alias{iplot}
\alias{outlier.plot}
\title{
Probabilistic Sliding Template Model
}
\description{
Classify Vowels Using the Probabilistic Sliding Template Model
}
\usage{
PSTM (ffs, f0, template, winner = TRUE)
}
\arguments{
  \item{ffs}{A matrix of formant frequencies in which each row represents a single vowel token, and each column represents a formant frequency. At least 3 formants need to be specified for every vowel.} 
  \item{f0}{A vector containing the average f0 measured for each vowel to be classified. The length of this vector must equal the number of rows in ffs.}
  \item{template}{A 'template' object created with the createtemplate() function provided in this package. If no template is specified, vowels are classified relative to vowels from Edmonton English.}
  \item{winner}{If TRUE, only the winner of each classification is returned. If FALSE, information regarding all candidate vowels is returned. See 'value' subsection for details.}
}
\details{
The classic log-mean normalization method of Nearey (1978) helps compare the vowels produced by different speakers by controlling for the log-mean formant frequency (FF) produced by a speaker. This approach to normalization assumes that variation in the vowel spaces of speakers of the same dialect is primarily according to a single multiplicative parameter. When this speaker-specific scaling parameter is controlled for, differences in the vowel spaces of different speakers are minimized. 

The Probabilistic Sliding Template Model (PSTM) of Nearey and Assmann (2007) attempts to predict perceived vowel quality by 'guessing' an appropriate speaker-specific scaling parameter and normalizing vowels using this estimated parameter. 'Method 6' of the PSTM (described in Nearey & Assmann, 2007) is used to estimate the necessary parameter. After normalization, vowels are classified by comparing them to a provided reference template, which can be created using the createtemplate() function included in this package. Normalized or unnormalized vowels may be classified, as long as the same transformations are performed on the data used to create the template and the data being classified.  

If no template is passed, the model identifies vowels relative to the vowel system of Edmonton English speakers. For in-depth details regarding the specifics of this model, please see Nearey & Assmann (2007).
}
\value{
If winner = TRUE:

A dataframe with the following columns:

\item{vowel}{The label for each winning vowel.}
\item{psi}{The optimal psi determined for the winning vowel.}
\item{postprob}{The posterior probability of observing the winning vowel, given the formants and the psi.}

If winner = FALSE:

A list of dataframes, each of which contains information for every candidate vowel category for each token to be classified. Each dataframe has the following columns:

\item{vowel}{The label for each candidate vowel category.}
\item{psi}{The optimal psi determined for each vowel category.}
\item{postprob}{The posterior probability of observing each vowel category, given the formants and the psi.}
}
\references{
Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD thesis, Indiana University Linguistics Club. 


Nearey, T. M. & P. F. Assmann. (2007). Pobabilistic 'sliding template' models for indirect vowel normalization. in Experimental Approaches to Phonology, edited by M.J. Sole, P. S., Beddor, and M. Ohala (Oxford University Press, Oxford), pp. 246-269.
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## load Peterson & Barney vowel data
data (pb52)

## normalize vowel formant frequencies
normdvowels = normalize (pb52[,7:9], pb52$speaker, pb52$vowel)
formants = normdvowels[,1:3]
vowels = pb52$vowel

## make a vowel template based on these frequencies
template = createtemplate (formants, vowels) 

## first classify only the first three vowels
ffs = pb52[1:3,c(7:9)] 
f0 = pb52[1:3,6]

## outputting only the winners, and then the full posterior probabilities
PSTM (ffs, f0, template)
PSTM (ffs, f0, template, winner = FALSE)

## now classify all vowels
## uncomment to run
#ffs = pb52[,c(7:9)] 
#f0 = pb52[,6]  
#winner = PSTM (ffs, f0, template)
## with a good degree of accuracy
#table (winner$vowel, pb52$vowel)
}

