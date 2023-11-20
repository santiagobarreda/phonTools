\name{p73}
\alias{p73}
\docType{data}
\title{
Pols et al. (1973) Dutch Vowel Data
}
\description{
Formant frequency information for vowels averaged across 24 male speakers. 
}
\usage{data (p73)}
\format{
  A data frame with the following columns:
  \describe{
    \item{\code{sex}}{ - A factor indicating speaker sex.}
    \item{\code{vowel}}{ - The vowel category in x-sampa.}
    \item{\code{f1}}{ - A numeric vector indcating the vowel F1 in Hz.}
    \item{\code{f2}}{ - A numeric vector indcating the vowel F2 in Hz.}
    \item{\code{f3}}{ - A numeric vector indcating the vowel F3 in Hz.}
  }
}
\references{
Pols, L. C. W., Tromp, H. R. C., & Plomp, R. (1973). Frequency analysis of Dutch vowels from 50 male speakers. Journal of the Acoustical Society of America, 53. 1093-1101. 
}
\examples{
data(p73)
vowelplot (p73$f1, p73$f2, p73$vowel, logaxes = 'xy', ellipses = TRUE, 
xsampa = TRUE)
}
