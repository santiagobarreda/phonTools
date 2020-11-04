\name{reduce.fraction}
\alias{reduce.fraction}
\title{
Reduce Fractions 
}
\description{
Reduce fractions to lowest terms using Euclid's Algorithm.
}
\usage{
reduce.fraction (ratio)
}
\arguments{
  \item{ratio}{A vector with two integers. The first element is the numerator of a ratio, and the second element is the denominator.} 
}
\value{
A vector containing the elements of the reduced fraction. The first element is the numerator of a ratio, and the second element is the denominator. 
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## an easy one
reduce.fraction (c(100, 200))

## irreducible
reduce.fraction (c(140, 201))

## a hard one
reduce.fraction (c(140, 203))
}

