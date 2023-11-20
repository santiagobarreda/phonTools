\name{a96}
\alias{a96}
\docType{data}
\title{
Aronson et al. (1996) Hebrew Vowel Data
}
\description{
Formant frequency information for vowels averaged across 6 male speakers. 
}
\usage{data (a96)}
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
Aronson, L., Rosenhouse, J. Rosenhouse, G. & Podoshin, L. (1996). An acoustic analysis of modern Hebrew vowels and voiced consonants. Journal of Phonetics 24. 283-193. 
}
\examples{
data(a96)
vowelplot (a96$f1, a96$f2, a96$vowel, logaxes = 'xy', xsampa = TRUE)
}
