% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{f99}
\alias{f99}
\title{Fourakis et al. (1999) Greek Vowel Data}
\format{
A data frame with the following columns:
\itemize{
  \item sex A factor indicating speaker sex.
  \item vowel The vowel category in x-sampa.
  \item f1 A numeric vector indicating the vowel F1 in Hz.
  \item f2 A numeric vector indicating the vowel F2 in Hz.
  }
}
\description{
Formant frequency information for vowels averaged across 5 male speakers.
}
\examples{

data(f99)
vowelplot (f99$f1, f99$f2, f99$vowel, logaxes = 'xy', xsampa = TRUE)

}
\references{
Fourakis, M., Botinis, A. & Katsaiti, M. (1999). Acoustic
characteristics of Greek vowels. Phonetica, 56. 28-43.
}