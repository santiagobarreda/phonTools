\name{phasor}
\alias{phasor}
\title{
Plot Phasors
}
\description{
Plot phasors representing one or more complex-valued variables.
}
\usage{
phasor (num, scaled = TRUE, add = FALSE, circle = FALSE, 
        xlim, ylim, ...)
}
\arguments{
  \item{num}{A complex-valued variable to be plotted as a phasor, or a vector of such variables.} 
  \item{scaled}{If TRUE, phasor magnitudes are scaled to 1.} 
  \item{add}{If TRUE, phasors are plotted on existing figure. If FALSE, a new plot is created.} 
  \item{circle}{If TRUE and scaled is TRUE, the unit circle is drawn.} 
  \item{xlim}{x-axis range.} 
  \item{ylim}{y-axis range.} 
  \item{...}{Additional arguments are passed to the internal call of 'plot' and 'arrows'.} 
}
\details{
Complex-valued variables may be created using the complex() function.
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

noise = rnorm (100)
phasors = fft(noise)

par (mfrow = c(1,3))
phasor (phasors)
phasor (phasors, circle = TRUE)
phasor (phasors, scaled = FALSE)

}





