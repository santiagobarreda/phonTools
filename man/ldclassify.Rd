\name{ldclassify}
\alias{ldclassify}
\title{
Linear Discriminant Classification
}
\description{
Classify items using linear discriminant analysis. 
}
\usage{
ldclassify (data, means, covariance, template = NULL, posterior = 'no')
}
\arguments{
  \item{data}{A matrix in which each row represents an item to be classified, and each column represents an observation from a variable describing the item.} 
  \item{means}{A matrix of means where each row is a mean vector specifying a candidate category. The number of columns must equal the number of columns in data.} 
  \item{covariance}{The pooled within-groups covariance matrix to be used for classification.}
  \item{template}{A 'Template' object may be passed instead of a mean and covariance.}
  \item{posterior}{If 'winner', the posterior probability of the winning category is returned. If 'all', the posterior of every category is returned.} 
}
\details{
This function classifies the items described in the data matrix by comparing them to the reference patterns for the different candidate categories represented in the means matrix. The category with the minimum Mahalanobis distance to the observed pattern (i.e., a given row of the data matrix) is selected as the winner. Mahalanobis distances are found with using the covariance matrix provided to the function. 

Mean and covariance matrices can be made easily for data using the createtemplate() function included in this package.
}
\value{
A vector of winning categories is returned. If winning posteriors are desired, these are returned in a second column. All posteriors are returned in separate columns for each category.
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

## classify vowels
answers = ldclassify (formants, template$means, template$covariance)
## compare to known categories
table (answers, vowels)
}

