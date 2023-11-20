\name{playsound}
\alias{playsound}
\alias{play}
\title{
Play Sounds
}
\description{
Play sounds in R using VLC Player.
}
\usage{
playsound  (sound, path = 'default', fs = 10000, erase = TRUE)
}
\arguments{
  \item{sound}{Either a sound object, or a numeric vector to be played.} 
  \item{path}{The location of VLC.exe on your computer. 'default' provides the default for a standard windows installation of VLC player. If this is set to 'pick', a dialog box opens allowing the user to specify the path.} 
  \item{fs}{The sampling frequency in Hz. If a 'sound' or 'ts' object is passed this does not need to be specified.}
  \item{erase}{If TRUE, the temporary WAV file created is deleted after playing.} 
}
\details{
The sound is written out as a .wav file and VLC is called from the command line to play the file. The file is optionally erased after the fact. If the path is selected by the user, this is returned for use in future calls. Obviously, this function relies on VLC being installed. With the appropriate path, it seems like this function should work on Linux and OSX, though it has only been tested on a Windows computer. 
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
#  ## make a 1 second, 100 Hz tone.
#  tone = sinusoid (freqs = 250, dur = 1000, fs = 1000)[,2]
#  ## play it in VLC player
#  playsound (q[,2], fs = 1000, erase = FALSE)
}





