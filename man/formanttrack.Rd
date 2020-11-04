\name{formanttrack}
\alias{formanttrack}
\title{
Formant Tracking
}
\description{
Create a formant track for a sound.
}
\usage{
formanttrack (sound, timestep = 5, windowlength = 30, 
formants = 5, cutoff = 5000, minformant = 200, maxbw = 600, 
fs = 22050, show = TRUE, periodicity = .5, returnbw = FALSE)
}
\arguments{
  \item{sound}{A numeric vector representing a waveform, or a 'sound' object created with the loadsound() or makesound() functions.} 
  \item{timestep}{ How far the analysis window will advance, in milliseconds. If this value is set to zero, the function is applied to the entire signal.} 
  \item{windowlength}{ The length of the analysis window. Longer windows will result in increased accuracy with decreased time-resolution.} 
  \item{formants}{The desired number of formants to be tracked. Depending on the sound and time point, fewer than this many may be found.}   
  \item{cutoff}{The maximum analysis frequency. May not be higher than the Nyquist frequency.}   
  \item{minformant}{Formants below this frequency are rejected.} 
  \item{maxbw}{The maximum bandwidth for accepted formants.}   
  \item{fs}{ The sampling frequency of the sound. If a 'sound' or 'ts' object is passed, this does not need to be specified.} 
  \item{show}{If TRUE, a plot is created which allows the user to visually inspect the process.}
  \item{periodicity}{A value between 0 and 1. Signal sections with corrected ACF values lower than this are not analyzed. Allows voiceless sections to be excluded from analysis.} 
  \item{returnbw}{If TRUE, estimated formant bandwidths are returned.}   
}
\details{
This function works by repeatedly calling findformants(), and periodicity is established using pitchtrack(), both of which are included in this package. When no formants are found, or if less than the desired number of formants are found, a value of zero is returned for that formant, at that time point. 

Tracked formants are presented over a greyscale spectrogram if show is TRUE. When plotting, different colors are used for each formant to allow the user to distinguish these.
}
\value{
A dataframe with the following elements is returned:

\item{time}{The time, in milliseconds, of the middle of the analysis window.}
\item{f#}{The formant frequency for formant number #, one column for each formant.}
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

data (sound)

par (mfrow = c(2,1), mar = c(4,4,1,1))
formanttrack (sound)
#formanttrack (sound, periodicity = 0)

}

