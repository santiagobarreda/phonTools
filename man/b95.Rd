% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{b95}
\alias{b95}
\title{Bradlow (1995) Spanish Vowel Data}
\format{
A data frame with the following columns: #' 
\itemize{
  \item sex A factor indicating speaker sex.
  \item vowel The vowel category in x-sampa.
  \item f1 A numeric vector indicating the vowel F1 in Hz.
  \item f2 A numeric vector indicating the vowel F2 in Hz.
  }
}
\description{
Formant frequency information for vowels averaged across 4 male speakers.
}
\examples{

data(b95)
vowelplot (b95$f1, b95$f2, b95$vowel, logaxes = 'xy', xsampa = TRUE)

}
\references{
Bradlow, A. R. (1995). A comparative acoustic study of English
and Spanish vowels. Journal of the Acoustical Society of America 97.
1916-1924.
}