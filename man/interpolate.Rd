\name{interpolate}
\alias{interpolate}
\title{
Interpolation
}
\description{
Piece-wise cubic or linear spline interpolation.
}
\usage{
interpolate (y, x = 1:length(y), steps = 20, increment = -1, 
show = FALSE, type = 'cubic', ...)
}
\arguments{
  \item{y}{A vector of 'knots', between which the function will interpolate points.}
  \item{x}{The 'x' coordinates corresponding to each knot. If not specified, the knots are assumed to be equally spaced.}
  \item{steps}{The number of interpolating steps between each knot. Increasing this number will result in a smoother interpolation. If the knots are not equally spaced along the x-axis, the interpolated points will not be equally spaced across the entire curve.}
  \item{increment}{If this is greater than 0, interpolated points are separated along the x-axis by this value. Note that if the knot locations are not multiples of this increment, there will be irregularities in the spacing of the interpolated points.}
  \item{show}{If TRUE, the result of the interpolation is shown in a plot.}
  \item{type}{If 'cubic', a natural cubic spline interpolation is performed. If 'linear', a linear interpolation is performed.} 
  \item{...}{Additional arguments are passed to the internal call of plot() if show = TRUE.}
}
\details{
By default, this function performs a 'natural' cubic spline interpolation between the points provided by the user. Optionally, a linear interpolation between the points may be carried out.
}
\value{
A dataframe with columns corresponding to the x and y dimensions of the interpolated points is returned.

\item{x}{The x-axis coordinates of the interpolated points.}
\item{y}{The y-axis coordinates of the interpolated points.}
}
\references{
http://en.wikipedia.org/wiki/Spline_interpolation
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## generate ten random points
y = rnorm (10, 0, 5)
interpolate (y, show = TRUE)  ## plot a cubic interpolation
linear = interpolate (y, type = 'linear')   
## and compare to a linear interpolation
lines (linear, col = 2) 
}
