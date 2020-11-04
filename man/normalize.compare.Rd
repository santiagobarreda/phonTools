\name{normalize.compare}
\alias{normalize.compare}
\title{
Compare Normalization Methods
}
\description{
Compare the effectiveness of different normalization methods. 
}
\usage{
normalize.compare (normd)
}
\arguments{
  \item{normd}{A list of dataframes containing the different formant data to be compared. Each dataframe must contain columns called "f1", "f2", "speaker", and "vowel", in any order.} 
}
\details{
This function provides a relatively straightforward way to compare the effectiveness of different normalization methods based on the assumption that a good normalization method maximizes the separation between different vowel categories and minimizes the variation within a single vowel category. Minimizing the variation within a single vowel category means that the vowel spaces of different speakers are maximally similar. 

This function provides two measures to compare the performance of normalization methods:

1) The square root of the average Mahalanobis distance between vowel categories is found for all pairs of vowel categories. This value indicates the average separation of vowel categories with respect to within-category error and the covariance patterns of the formant frequencies. A higher value indicates a better performing normalization algorithm.  

2) The percentage of correct classifications using a linear discriminant model trained on the normalized formant-frequencies using the given category-labels. 
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## load the Peterson and Barney vowels
data (pb52)

## normalize using several different methods
neareyE = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, 
method = 'neareyE')
neareyI = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, 
method = 'neareyI')
lobanov = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, 
method = 'lobanov')
wandf = normalize (pb52[,7:9], pb52$speaker, pb52$vowel, 
method = 'wandf', corners =  c('i','A'))
normd = list (pb52, neareyE, neareyI, lobanov, wandf)

## compare outcome of methods (and unnormalized vowels)
## uncomment to run
#normalize.compare (normd)
}
