\name{f99}
\alias{f99}
\docType{data}
\title{
Fourakis et al. (1999) Greek Vowel Data
}
\description{
Formant frequency information for vowels averaged across 5 male speakers. 
}
\usage{data (f99)}
\format{
  A data frame with the following columns:
  \describe{
    \item{\code{sex}}{ - A factor indicating speaker sex.}
    \item{\code{vowel}}{ - The vowel category in x-sampa.}
    \item{\code{f1}}{ - A numeric vector indcating the vowel F1 in Hz.}
    \item{\code{f2}}{ - A numeric vector indcating the vowel F2 in Hz.}
  }
}
\references{
Fourakis, M., Botinis, A. & Katsaiti, M. (1999). Acoustic characteristics of Greek vowels. Phonetica, 56. 28-43.
}
\examples{
data(f99)
vowelplot (f99$f1, f99$f2, f99$vowel, logaxes = 'xy', xsampa = TRUE)
}
