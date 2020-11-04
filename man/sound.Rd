\name{sound}
\alias{sound}
\docType{data}
\title{
Sound object
}
\description{
An example of a sound object, the phrase 'This is a spectrogram', produced by the author of this package. This sound object may be inspected with spectrogram() and plotted with plot(). Sections of the individual samples representing the waveform (found in the "sound" element of a sound object) can be passed to spectralslice() to see a spectral slice. Several functions included in this package may also be used to modify or manipulate 'sound' objects.
}
\usage{data (sound)}
\format{
This sound object has the same properties as all sound objects. These may be inspected by using the print() function.
}
\examples{

## uncomment and run

#data (sound)
#par (mar = c(4,4,1,1))
#multiplot (n = 3, sizes = c(.25, .5, .25))  

#plot (sound)
#spectrogram (sound, dynamicrange = 50, maxfreq = 7000)
}
