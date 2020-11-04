\name{ntypes}
\alias{ntypes}
\title{
Number of Unique Elements
}
\description{
Find the number of unique elements in a vector.
}
\usage{
ntypes (vector) 
}
\arguments{
  \item{vector}{The vector of interest.} 
}
\details{
A simple function that converts a vector to a factor, and finds the number of levels. This provides the number of unique elements in a vector, something I find I frequently need. 
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
data (pb52)
## the number of unique vowel categories.
ntypes (pb52$vowel)
}
