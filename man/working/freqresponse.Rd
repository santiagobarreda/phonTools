\name{freqresponse}
\alias{freqresponse}
\title{
Frequency Response
}
\description{
Find the frequency response of a digital filter
}
\usage{
freqresponse (b, a, fs = 0, add = FALSE, show = TRUE,
              steps = 1000,...)
}
\arguments{
  \item{b}{The moving-average (MA), numerator coefficients of the filter.} 
  \item{a}{The autoregressive (AR), denominator coefficients of the filter. Please note that the leading 1 at a[0] is not assumed.} 
  \item{fs}{The sampling frequency of the sound. If this is not given calculations are presented as if fs = 1.} 
  \item{add}{If TRUE, the frequency response plot is added to an existing plot.} 
  \item{show}{If TRUE, the frequency response of the estimated filter is plotted.} 
  \item{steps}{The number of steps between zero and the Nyquist frequency.} 
  \item{...}{Additional arguments are passed to internal plotting functions.} 
}
\details{
This function plots (and optionally returns) the frequency response for the digital filter whose transfer function is determined by the numerator and denominator filter coefficients given in b and a.
}
\value{
A dataframe with two columns (frequency and response) that can be used to redraw the frequency response if required. The 'response' value corresponds to dB. magnitude below peak. 
}
\references{
Lyons, R. G. (2004). Understanding Digital Signal Processing (2nd ed.). Prentice Hall.
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

## make a synthetic vowel with a known set of formant frequencies
sound = vowelsynth (ffs = c(500,1500,2500,3500,4500), 
fbw = c(30, 80, 150, 200, 220),f0 = 100, dur = 100)

plot (sound)

## let the LPC function estimate the filter used to generate the vowel
coeffs = lpc (sound, show = FALSE)

## compare frequency response of estimated filter to vowel spectrum
spectralslice (sound, col = 4, preemphasisf = 50)
freqresponse (1, coeffs, add = TRUE, fs = 10000)

## generate a sinc function
filt = sinc (seq (-15,15,1/2), normalized = TRUE)
## treat it as a low-pass FIR filter and inspect its frequency response
freqresponse (filt, 1)

}
