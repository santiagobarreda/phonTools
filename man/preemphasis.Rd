\name{preemphasis}
\alias{preemphasis}
\title{
Add Preemphasis
}
\description{
Single-zero preemphasis filter.
}
\usage{
preemphasis (input, cutoff = 50, fs = 22050, verify = FALSE, 
coeff = 0)
}
\arguments{
  \item{input}{Either a numeric vector representing a sequence of samples taken from a sound wave or a sound object created with the loadsound() or makesound() functions.}
  \item{cutoff}{The spectral slope is increased by 6 dB. per octave above this frequency.} 
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed, this does not need to be specified.} 
  \item{verify}{If TRUE, before and after spectra are plotted to allow the user to visually verify the process.} 
  \item{coeff}{Optionally, the single coefficient used by the filter may be specified.} 
}
\value{
The modified sound is returned. If a 'sound' object another 'sound' object is returned. 
}
\references{

http://www.fon.hum.uva.nl/praat/manual/Sound__Filter__pre-emphasis____.html

}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
signal = sinusoid (c(100, 200, 400, 800, 1600), fs = 4000, 
dur = 100, sum = TRUE)[,7]

preemphasis (signal, verify = TRUE, fs = 4000, cutoff = 50)
}
