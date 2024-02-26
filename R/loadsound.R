
#' Load WAV files into R
#' 
#' A function which allows WAV files to be loaded into R.
#' 
#' The function is only compatible with 8 and 16 bit mono WAV files. The
#' function returns a 'sound object'. Many of the functions included in this
#' package interact with 'sound' objects.
#' 
#' @export
#' @aliases loadsound print.sound plot.sound
#' @param filename A string indicating the file name of the WAV file to be
#' loaded. If no filename is provided, a dialog box will open allowing the user
#' to select a file.
#' @param channel A string indicating the channel to be loaded. The default is
#' 'left'. If 'right' is selected, the right channel will be loaded.
#' @return An object of class 'sound', a list containing the elements:
#' 
#' \item{filename}{A vector containing the filename of the WAV file.}
#' \item{fs}{The sampling frequency of the sound. } 
#' \item{duration}{The duration of the sound, in milliseconds. } 
#' \item{sound}{A vector of numeric values representing the sampled sound. }
#' \item{bit}{The wav file bit rate. }
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
#' @examples
#' 
#' ## Use the command below to select a WAV file to load into R
#' ## sound = loadsound ()
#' 
#' ## sound
#' ## plot (sound)
#' ## spectrogram (sound)
#' 
 
loadsound = function (filename=NA, channel = 'left'){
  if (is.na(filename)) filename = file.choose()

  snd = tuneR::readWave (filename)
  
  numSamples = length(snd@left)
  fs = snd@samp.rate
  
  sound = snd@left
  if (channel == 'right') sound = snd@right

  output = list (filename = filename, fs = fs, numSamples = numSamples,
                 duration = numSamples/fs * 1000, sound = ts(sound, frequency = fs, start=0),
                 bit = snd@bit)
  class(output) = "sound"
  output  
}
