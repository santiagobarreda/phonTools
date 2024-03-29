% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lpc.R
\name{lpc}
\alias{lpc}
\title{Linear Predictive Coding}
\usage{
lpc(
  sound,
  order = round(fs/1000) + 3,
  fs = 10000,
  show = FALSE,
  add = FALSE,
  preemph = TRUE
)
}
\arguments{
\item{sound}{Either a numeric vector representing a sequence of samples
taken from a sound wave or a sound object created with the loadsound() or
makesound() functions.}

\item{order}{The number of LPC coefficients to be estimated. By default
there is 2 per kHz below the Nyquist frequency plus 3 extra coefficients.}

\item{fs}{The sampling frequency in Hz. If a sound object is passed this
does not need to be specificed.}

\item{show}{If TRUE, the frequency response of the estimated filter is
plotted.}

\item{add}{If TRUE, the frequency response plot is added to an existing
plot.}

\item{preemph}{If TRUE, preemphasis of 3 dB per octave is applied to the
sound before analysis.}
}
\value{
A vector containing the LPC coefficients is returned.
}
\description{
Predict autoregressive filter coefficients.
}
\details{
LPC coefficients are estimated using the autocorrelation method. The signal
is windowed with a Hanning window prior to analysis.
}
\examples{

## make a synthetic vowel with a known set of formant frequencies
sound = vowelsynth (ffs = c(500,1500,2500,3500,4500), 
fbw = c(30, 80, 150, 200, 220),f0 = 100, dur = 250)

## let the LPC function estimate the filter used to generate the vowel
coeffs = lpc (sound, show = TRUE)

}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
