\name{t07}
\alias{t07}
\docType{data}
\title{
Thomson (2007) Vowel Data
}
\description{
Mean formant frequency information for the vowels of male and female speakers from Edmonton English.
}
\usage{data (pb52)}
\format{
  A data frame with 1520 observations on the following 9 variables:
  \describe{
    \item{\code{vowel}}{ - The vowel category in x-sampa.}
    \item{\code{gender}}{ - A factor with levels \code{f} \code{m} representing speaker gender: male and female.}
    \item{\code{f1}}{ - A numeric vector indcating the vowel F1 in Hz.}
    \item{\code{f2}}{ - A numeric vector indcating the vowel F2 in Hz.}
    \item{\code{f3}}{ - A numeric vector indcating the vowel F3 in Hz.}
  }
}
\references{
Thomson, R. (2007). Modeling L1/L2 interactions in the perception and production of English vowels by Mandarin L1 speakers: A training study. PhD dissertation, University of Alberta.
}
\examples{
data(t07)
vowelplot (t07$f1, t07$f2, t07$vowel, logaxes = 'xy', meansOnly = TRUE, 
xsampa = TRUE)
}
