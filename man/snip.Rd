\name{snip}
\alias{snip}
\title{
Snip/Zoom
}
\description{
Select a subsection of a sound or spectrogram object.
}
\usage{
snip  (object, show = TRUE)
}
\arguments{
  \item{object}{A 'sound' or 'spectrogram' object to be truncated.} 
  \item{show}{If TRUE, the selected subsection is displayed, resulting in zoom functionality.} 
}
\details{
The input object is plotted and the user must click on two points on the plot. If show = TRUE, this function allows the user to zoom in on a subsection of the object. If the output is assigned to a variable, a new object is created and returned that contains only the data in between the two designated points. 
}
\value{
A 'sound' or 'spectrogram' object which interacts with several functions included in this package.
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
data (sound)
## the example below is commented because examples cannot 
## require user interaction. simply uncomment the lines below
## and select two points on the plot. 
# clipped = snipsound (sound)
# spectrogram (clipped)
# spectralslice (clipped)
}
