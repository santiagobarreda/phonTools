% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lowpass.R
\name{lowpass}
\alias{lowpass}
\title{Low pass filter a signal}
\usage{
lowpass(sound, cutoff = 0.5, order = 6, n_passes = 4)
}
\arguments{
\item{sound}{A numeric vector containing the signal to be filtered.}

\item{order}{The order of the filter to be used.}

\item{n_passes}{The number of times the filter is applied to the signal.}

\item{r}{The cutoff frequency of the filter where r = cutoff/Nyquist frequency (Nyquist=fs/2).}
}
\value{
A numeric vector containing the filtered signal.
}
\description{
Low pass filters a signal using a cascaded butterworth filters.
}
\examples{
\dontrun{
x = rnorm(1000)
filtered_x = lowpass (x, 0.5)
phonTools::spectralslice (x, ylim = c(-200,20), fs=1, col=4)
phonTools::spectralslice (filtered_x, ylim = c(-200,20), add = TRUE, col = 2)
abline (v=0.25)
}
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
