\name{selectslice}
\alias{selectslice}
\title{
Select Slices
}
\description{
Select spectral slices from a spectrogram.
}
\usage{
selectslice (specobject, n = 1, plot = TRUE, ...)
}
\arguments{
  \item{specobject}{A 'spectrogram' object created with the spectrogram() function included in this package.} 
  \item{n}{The number of desired slices.} 
  \item{plot}{If FALSE, the spectrogram is not plotted.} 
  \item{...}{Additional arguments are passed to the internal call of plot.spectrogram().} 
}
\details{
This function allows the user to select a given number of time points on a spectrogram, and to retrieve spectral slices at those time points. The spectrogram object is plotted, and the user must click on the spectrogram a given number of times at the desired points. The analysis parameters of the slices will reflect the parameters used when creating the spectrogram. Setting the 'plot' parameter to FALSE is useful if you want one set of analysis parameters for the visually presented spectrogram and another set for the spectral slices.
}
\value{
A dataframe with one column for each spectral slice selected. Row names indicate frequencies, column names indicate times in milliseconds. Values are decibels below peak. 
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

## uncomment to run

# sound = vowelsynth ()
# spect = spectrogram (sound)
# slices = selectslice (spect, n = 3)

}





