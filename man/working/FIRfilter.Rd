\name{FIRfilter}
\alias{FIRfilter}
\title{
Perform Digital Filtering
}
\description{
Finite Impulse Response (FIR) filtering of vectors.
}
\usage{
FIRfilter (sound, from = 0, to = fs/2, fs = 22050, order = 200, 
verify = FALSE, impulse = NULL, pad = TRUE)
}
\arguments{
  \item{sound}{A numeric vector representing a waveform, or a 'sound' object created with the loadsound() or makesound() functions.} 
  \item{from}{The low cutoff point for the filter. Frequencies higher than this are not attenuated. Minimum allowed value is 0 Hz.} 
  \item{to}{The high cutoff point for the filter. Frequencies lower than this are not attenuated. Maximum allowed value is fs/2 Hz.} 
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed, this does not need to be specified.} 
  \item{order}{The number of taps included in the filter. The order of the filter may not exceed the number of samples that make up the sound.} 
  \item{verify}{If TRUE, a plot comparing the spectrum of the input sound is compared the the filtered sound.} 
  \item{impulse}{If a filter impulse response is specified, this is used to filter the signal.} 
  \item{pad}{If TRUE, the signal is padded with zero so that the original length is maintained.} 
}
\details{
This function performs lowpass, highpass and bandpass filtering using a windowed-sinc filter. Increasing the filter order decreases the transition region between the passband and the stopband. The magnitude of frequencies in the stopband is usually attenuated by about about 45 dB.

If verify is TRUE, a plot is created which allows the user to inspect the performance of the function. 
}
\value{
If a vector is given, the filtered vector is returned.

If a 'sound' object is given, a sound object containing the filtered sound is returned. 
}
\references{
http://en.wikipedia.org/wiki/Sinc_filter

Lyons, R. G. (2004). Understanding Digital Signal Processing (2nd ed.). Prentice Hall.
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## generate random noise
#sound = rnorm (5000, 0, 100)

## implement low, high and band-pass filters
#par (mfrow = c(3,1), mar = c(4,4,1,1))
#snd1 = FIRfilter (sound, from = 400, fs = 1000, verify = T)
#snd2 = FIRfilter (sound, to = 400, fs = 1000, verify = T)
#snd3 = FIRfilter (sound, from = 400, to = 100, fs = 1000, verify = T)

## use filters of different orders (i.e., differing number of taps)
#par (mfrow = c(2,1), mar = c(4,4,1,1))
#snd1 = FIRfilter (sound, to = 400, fs = 1000, order = 50, verify = T)
## higher order filters lead to narrower transition regions
#snd2 = FIRfilter (sound, to = 400, fs = 1000, order = 2000, verify = T)
}
