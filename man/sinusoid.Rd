\name{sinusoid}
\alias{sinusoid}
\alias{sinusoids}
\title{
Create Sinusoids
}
\description{
Create and display one or more sinusoids.
}
\usage{
sinusoid (freqs, amps = rep(1, length(freqs)), dur = 50, 
phases = rep(0, length(freqs)), fs = 10000, sum = FALSE, 
show = FALSE, colors = NULL)
}
\arguments{
  \item{freqs}{A vector of frequencies, one for each desired sinusoid.} 
  \item{amps}{A vector of peak amplitudes, one for each desired sinusoid.} 
  \item{dur}{The desired duration of the sinusoids, in milliseconds.} 
  \item{phases}{A vector of initial phases, one for each desired sinusoid, expressed in radians.} 
  \item{fs}{The desired sampling frequency of the sinusoids.} 
  \item{sum}{If TRUE, the sum the generated sinusoids is also found.} 
  \item{show}{If TRUE, the generated sinusoids are plotted. If sum is TRUE, a second plot is created to display the sum of the sinusoids.} 
  \item{colors}{An optional vector of colors used to plot the individual sinusoids. If the number of colors given is less than the number of frequencies specified, the colors are repeated.} 
}
\details{
A number of sinusoids are generated, and optionally plotted and/or returned. The number of frequencies specified must equal the number of amplitudes and initial phases. 
}
\value{
A dataframe with the following columns:

\item{time}{The time, in milliseconds, at which is sample is taken.}
\item{waveN}{A series of columns, each indicating the amplitude of wave N at a given time.}
\item{sum}{A column indicating the sum of all of the sinusoids.}
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## several waves, and the sum
sum = sinusoid (freqs = c(100,200,300), amps = c(1,3,2), 
sum = TRUE, show = TRUE)

## no sum, different phase shifts
nosum = sinusoid (freqs = c(100,200,300), amps = c(1,3,2), 
phases = c(pi/2, 0, pi/4), sum = FALSE)
}
