\name{f73}
\alias{f73}
\docType{data}
\title{
Fant (1973) Swedish Vowel Data
}
\description{
Formant frequency information for vowels averaged across 50 male speakers. 
}
\usage{data (f73)}
\format{
  A data frame with the following columns:
  \describe{
    \item{\code{sex}}{ - A factor indicating speaker sex.}
    \item{\code{vowel}}{ - The vowel category in x-sampa.}
    \item{\code{f1}}{ - A numeric vector indcating the vowel F1 in Hz.}
    \item{\code{f2}}{ - A numeric vector indcating the vowel F2 in Hz.}
    \item{\code{f3}}{ - A numeric vector indcating the vowel F3 in Hz.}
    \item{\code{f4}}{ - A numeric vector indcating the vowel F4 in Hz.}
  }
}
\references{
Fant, G. (1973). Speech sounds and features. Cambridge, MA: MIT Press.
}
\examples{
data(f73)
vowelplot (f73$f1, f73$f2, f73$vowel, logaxes = 'xy', xsampa = TRUE)
}
