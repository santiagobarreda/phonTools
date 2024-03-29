% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/powertrack.R
\name{powertrack}
\alias{powertrack}
\title{Power tracking}
\usage{
powertrack(
  sound,
  timestep = 5,
  windowlength = 30,
  fs = 22050,
  show = TRUE,
  zeromax = TRUE,
  ...
)
}
\arguments{
\item{sound}{A numeric vector representing a waveform, or a 'sound' object
created with the loadsound() or makesound() functions.}

\item{timestep}{Determines how far the window will be moved for each
adjacent analysis, in milliseconds.}

\item{windowlength}{Determines how much of the signal is included for each
analysis step, in milliseconds. If this is too small, pitch-synchronous
ripples will be seen in the track.}

\item{fs}{The sampling frequency of the sound. If a 'sound' or 'ts' object
is passed, this does not need to be specified.}

\item{show}{If TRUE, the track is plotted.}

\item{zeromax}{If TRUE, the maximum dB value is set to zero.}

\item{...}{Additional parameters are passed to the internal call of plot(),
and used to create the figure.}
}
\value{
A dataframe with the following columns:

\item{time }{ The time, in milliseconds, of each point of analysis.}
\item{power }{ The power, in decibels, at each analysis point.}
}
\description{
Create a power track for a sound.
}
\details{
Returns the average power in the signal in a section as determined by the
parameters of the function. A Hann window is applied to each section prior
to analysis. Sections with zero power are ignored and not returned.
}
\examples{


## plot the waveform and power of a given sound.
data (sound)
# sound = loadsound()        ## run this line to use you own sound

## compare waveform and power
par (mfrow = c(2,1), mar = c(4,4,1,1));
plot (sound)
powertrack (sound)

}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
