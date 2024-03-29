% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{a96}
\alias{a96}
\title{Aronson et al. (1996) Hebrew Vowel Data}
\format{
A data frame with the following columns: 
\itemize{
  \item sex A factor indicating speaker sex.
  \item vowel The vowel category in x-sampa.
  \item f1 A numeric vector indicating the vowel F1 in Hz.
  \item f2 A numeric vector indicating the vowel F2 in Hz.
  \item f3 A numeric vector indicating the vowel F3 in Hz.
  \item f4 A numeric vector indicating the vowel F4 in Hz.
  }
}
\description{
Formant frequency information for vowels averaged across 6 male speakers.
}
\examples{

data(a96)
vowelplot (a96$f1, a96$f2, a96$vowel, logaxes = 'xy', xsampa = TRUE)

}
\references{
Aronson, L., Rosenhouse, J. Rosenhouse, G. & Podoshin, L.
(1996). An acoustic analysis of modern Hebrew vowels and voiced consonants.
Journal of Phonetics 24. 283-193.
}
