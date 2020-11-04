\name{loadsound}
\alias{loadsound }
\alias{print.sound}
\alias{plot.sound}
\title{
Load WAV files into R
}
\description{
A function which allows WAV files to be loaded into R. 
}
\usage{
loadsound (filename)
}
\arguments{
  \item{filename}{A string indicating the file name of the WAV file to be loaded. If no filename is provided, a dialog box will open allowing the user to select a file.}
}
\details{
The function is only compatible with 8 and 16 bit mono WAV files. The function returns a 'sound object'. Many of the functions included in this package interact with 'sound' objects.
}
\references{
https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
}
\value{
An object of class 'sound', a list containing the elements:

\item{filename}{A vector containing the filename of the WAV file.}
\item{fs}{The sampling frequency of the sound. }
\item{duration}{The duration of the sound, in milliseconds. }
\item{sound}{A vector of numeric values representing the sampled sound. }
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## Use the command below to select a WAV file to load into R
## sound = loadsound ()

## sound
## plot (sound)
## spectrogram (sound)
}
