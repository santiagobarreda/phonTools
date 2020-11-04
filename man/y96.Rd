\name{y96}
\alias{y96}
\docType{data}
\title{
Yang (1996) Korean Vowel Data
}
\description{
Formant frequency information for vowels averaged across 60 male and female speakers. 
}
\usage{data (y96)}
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
Yang, B. (1996). A comparative study of American English and Korean vowels produced by male and female speakers. Journal of Phonetics, 24. 245-261.
}
\examples{
data(y96)
vowelplot (y96$f1, y96$f2, y96$vowel, logaxes = 'xy', xsampa = TRUE)
}
