\name{pwelch}
\alias{pwelch}
\title{
Welch's Power Spectral Density Estimate
}
\description{
Calculates a power spectral density estimate using Welch's method.
}
\usage{
pwelch (sound, points = 0, overlap = 0, padding = 0, 
window = 'hamming', show = TRUE, fs = 1, preemphasisf = 0, 
zeromax = TRUE, type,...)
}
\arguments{
  \item{sound}{A vector representing a sound wave, or a 'sound' object.}
  \item{points}{The number of points to be included in each window. If not specified, the sound is divided into 10 equally-sized windows.} 
  \item{overlap}{The amount of overlap between adjacent segments, in points.} 
  \item{padding}{The amount of zero padding to be applied to each window.} 
  \item{window}{The window to be applied to each segment of the signal. See windowfunc() for available options.} 
  \item{show}{If FALSE, no plot is created.}
  \item{fs}{The sampling frequency of the sound. If a 'sound' object is passed this does not need to be specified.}
  \item{preemphasisf}{Preemphasis of 3 dB per octave is added to frequencies above the specified frequency.}
  \item{zeromax}{If TRUE, the maximum dB is set to 0.}
  \item{type}{The line type to be used for plotting, passes its value to the 'lty' parameter.}
  \item{...}{any additional arguments will be passed to the internal calls of 'plot'.} 
}
\details{
This function divides the signal into a number of equally-sized windows, finds the power spectrum of each one, and then finds the average across all windowed sections. 
}
\value{
The following columns:

\item{hz}{The center frequency of the analysis bins.}
\item{dB}{The dB magnitude/power for the analysis bin.}
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

## make a sine wave
sinewave = sinusoid (f = 300, fs = 1000, dur = 2000, sum = FALSE)

## add noise
sinewave = sinewave[,2] + rnorm (length (sinewave[,2]), 0, 3)

## compare the results of pwelch() and spectralslice()
par (mfrow = c(2,1), mar = c(4,4,1,1))
spectralslice (sinewave, fs = 1000)
pwelch (sinewave, points = 400, fs = 1000)

}





