\name{polezero}
\alias{polezero}
\title{
Pole-zero Plots
}
\description{
Generate a Pole-zero plot from filter coefficients.
}
\usage{
polezero (b, a, ...)
}
\arguments{
  \item{b}{The filter moving-average (MA) coefficients.} 
  \item{a}{The filter autoregressive (AR) coefficients.} 
  \item{...}{Additional parameters are passed to the internal call of plot().} 
}
\details{
This function plots filter poles (x) and zeros (o) based on the given coefficients.
}
\references{
http://en.wikipedia.org/wiki/Pole%E2%80%93zero_plot
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## example of a typical single-zero preemphasis filter
a = 1
b = c(1, -.94)
polezero (b, a)

#example of a complex-pole formant-style filter
a = c(1, -.3, .2)
b = c(1)
polezero (b, a)
}
