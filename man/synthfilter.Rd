\name{synthfilter}
\alias{synthfilter}
\title{
Filtering by Synthesis
}
\description{
Use Fourier synthesis to recreate signal without undesired frequency components.  
}
\usage{
synthfilter (sound, band = c(0,fs/4), fs = 1, verify = FALSE, attenuation = 0)
}
\arguments{
  \item{sound}{A numeric vector representing a waveform, or a 'sound' object created with the loadsound() or makesound() functions.} 
  \item{band}{A vector with exactly two elements, the first specifying the lowest frequency to be passed (must be > 0), and the second specifying the highest frequency to be passed (must be < fs/2).} 
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed, this does not need to be specified.} 
  \item{attenuation}{Attenuation of stop band, in dBs. If left as 0, stopband frequency components are completely omitted.}
  \item{verify}{If TRUE, a plot comparing the spectrum of the input sound is compared the the filtered sound.} 
}
\details{
This function performs lowpass, highpass and bandpass filtering by performing an FFT on the entire signal, zeroing out coefficients representing undesired frequency components, and performing an IFFT on the result. This approach may not be appropriate for some applications, but it is useful in some cases. This may be slow for long signals. 

If verify is TRUE, a plot is created which allows the user to inspect the performance of the function.
}
\value{
If a vector is given, the filtered vector is returned. If a 'sound' object is given, a sound object containing the filtered sound is returned. 
}

\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

## uncomment and run
##  white noise
noise = rnorm(5000)

## low-pass filter
#synthfilter (noise, band = c(0, .25), verify = TRUE)

##  band-pass filter
synthfilter (noise, band = c(.15, .25), verify = TRUE)

##  high-pass filter
#synthfilter (noise, band = c(.35, .5), verify = TRUE)

}
