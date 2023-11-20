\name{pb52}
\alias{pb52}
\docType{data}
\title{
Peterson & Barney (1952) Vowel Data
}
\description{
Formant frequency and f0 information for vowels collected from 76 speakers in the Peterson & Barney (1952) data.
}
\usage{data (pb52)}
\format{
  A data frame with 1520 observations on the following 9 variables:
  \describe{
    \item{\code{type}}{ - A factor with levels \code{c} \code{m} \code{w} representing speaker type: child, man and woman.}
    \item{\code{sex}}{ - A factor with levels \code{f} \code{m} representing speaker gender: male and female.}
    \item{\code{speaker}}{ - A numeric vector indicating a uniquely identifying speaker number.}
    \item{\code{vowel}}{ - The vowel category in x-sampa.}
    \item{\code{repetition}}{ - A numeric vector indicating the repetition number.}
    \item{\code{f0}}{ - A numeric vector indcating the vowel f0 in Hz.}
    \item{\code{f1}}{ - A numeric vector indcating the vowel F1 in Hz.}
    \item{\code{f2}}{ - A numeric vector indcating the vowel F2 in Hz.}
    \item{\code{f3}}{ - A numeric vector indcating the vowel F3 in Hz.}
  }
}
\source{
The data was created from tables provided within Praat:

http://www.fon.hum.uva.nl/praat/
}
\references{
Peterson, G.E. & Barney (1952). Control methods used in a study of the vowels. Journal of the Acoustical Society of America 24: 175-184.

Boersma, Paul & Weenink, David (2012). Praat: doing phonetics by computer [Computer program]. Version 5.3.19, retrieved 24 June 2012 from http://www.praat.org/
}
\examples{
data(pb52)
vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', ellipses = TRUE, 
xsampa = TRUE)
}
