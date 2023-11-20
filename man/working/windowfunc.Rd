\name{windowfunc}
\alias{windowfunc}
\title{
Window Function
}
\description{
Generates a window function of a given type and length.
}
\usage{
windowfunc (npoints, type = 'hann', parameter = -1)
}
\arguments{
  \item{npoints}{The desired window length, in points. If a vector is given, the window will have the same length as the vector.}
  \item{type}{A string indicating the type of window desired. For the sake of simplicity, all window names are in lowercase. Supported types are: rectangular, hann, hamming, cosine, bartlett, gausian, and kaiser.}
  \item{parameter}{The parameter necessary to generate the window, if appropriate. At the moment, the only windows that require parameters are the Kaiser and Gaussian windows. By default, these are set to 2 for kaiser and 0.4 for gaussian windows.}
}
\details{
A window function is generated, of the type and length specified by the user. This is returned as a numeric vector.
}
\references{
http://en.wikipedia.org/wiki/Window_function
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
par (mfrow = c(1,4))
plot (windowfunc (100, 'hann'))
plot (windowfunc (100, 'bartlett'))
plot (windowfunc (100, 'kaiser', parameter = 2))
plot (windowfunc (100, 'kaiser', parameter = 6))
}

