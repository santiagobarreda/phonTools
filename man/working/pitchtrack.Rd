\name{pitchtrack}
\alias{pitchtrack}
\title{
Pitch Tracking
}
\description{
Create a pitch track for a sound.
}
\usage{
pitchtrack (sound, f0range = c(60,400), timestep = 2, fs = 22050, minacf = .5,
correction = TRUE, show = TRUE, windowlength = 50, addtospect = FALSE)
}
\arguments{
  \item{sound}{ A numeric vector representing a waveform, or a 'sound' object created with the loadsound() or makesound() functions.} 
  \item{f0range}{ A numeric vector of length two where the first value corresponds to the minimum f0 Hz to be considered, and the second represents the maximum to be considered.} 
  \item{timestep}{ How far the analysis window will advance, in milliseconds. If this value is set to zero, the function is applied to the entire signal.} 
  \item{fs}{ The sampling frequency of the sound. If a 'sound' or 'ts' object is passed, this does not need to be specified.} 
  \item{minacf}{ Autocorrelation values below this are ignored.} 
  \item{correction}{ If TRUE, ACF values are corrected for lag value.} 
  \item{show}{ If TRUE, a plot displaying the pitch track is created.} 
  \item{windowlength}{ The length of the analysis window, in milliseconds. This should be approximately three-times longer than the wavelength of the lowest pitch. The default value is appropriate for a floor of 60 Hz.} 
  \item{addtospect}{ If TRUE, the pitch track is added to a spectrogram created with the spectrogram() function included in this package. The track is scaled up by a factor of 10 (e.g., 100 Hz will be plotted at 1000 Hz on the spectrogram) so that it will fit nicely in the typical spectrogram range of 0-5000 Hz.} 
}
\details{
Pitch tracking is carried out using a simplified version of the algorithm described in Boersma (1993), including corrections for lag value and window function. When plotting pitch tracks, the points sizes are proportional to autocorrelation values.  
}
\value{ 
A dataframe with the following columns:

\item{time}{ The analysis window centre point, in milliseconds.}
\item{f0}{ The calculated f0 (pitch).}
\item{acf}{ The value of the autocorrelation function corresponding to the winning f0.}
}
\references{
Boersma, P., (1993). Accurate short-term analysis of the fundamental frequency and the harmonics-to-noise ratio of a sampled sound. Proc. Instit. Phon. Sci. 17: 97-110.
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

data (sound)               ## use the example 'sound' object provided
#sound = loadsound()       ## or run this line to use you own sound

## to generate a pitch track
output = pitchtrack (sound)

## to add a pitch to a spectrogram
#spectrogram (sound)
#pitchtrack (sound, addtospect = TRUE)
 
}





