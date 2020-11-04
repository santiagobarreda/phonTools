\name{b95}
\alias{b95}
\docType{data}
\title{
Bradlow (1995) Spanish Vowel Data
}
\description{
Formant frequency information for vowels averaged across 4 male speakers. 
}
\usage{data (b95)}
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
Bradlow, A. R. (1995). A comparative acoustic study of English and Spanish vowels. Journal of the Acoustical Society of America 97. 1916-1924.
}
\examples{
data(b95)
vowelplot (b95$f1, b95$f2, b95$vowel, logaxes = 'xy', xsampa = TRUE)
}
