
#' Make a 'sound' object
#' 
#' Create a 'sound' object from a numeric vector.
#' 
#' This function can make a vector into a 'sound' object. If a filename is not
#' set, the filename defaults to 'sound.wav' where 'sound' indicates the name
#' of the sound variable that was passed to the function. The benefit of
#' working with 'sound' objects is that they carry their sampling frequency and
#' filename (as well as some other information) with them.
#' 
#' @export
#' @param sound A numeric vector representing a sound wave.
#' @param filename A string indicating the desired file name associated with
#' this object.
#' @param fs The desired sampling frequency of the sound object.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
#' @examples
#' 
#' 
#' data (sound)
#' ## take only the first 10000 samples from a 'sound' object
#' tmp = sound$sound[1:10000]
#' ## and make a new 'sound' object
#' tmp = makesound (tmp, fs = 22050)
#' tmp
#' 
#' ## get ready to make two plots with thin margins
#' #multiplot (2); par (mar = c(4,4,1,1));
#' ## and show a spectrogram of the original
#' #spectrogram (sound)       
#' 
#' ## and the new, truncated version
#' #spectrogram (tmp)         
#' 
#' 
makesound = function (sound, filename, fs = 22050){
  if (missing(filename))filename = paste (deparse(substitute(sound)), '.wav', sep='')
  if (!is.numeric(sound)) stop('The sound must be a numeric vector.')

  numSamples = length(sound)
  output = list(filename = filename, fs = fs, numSamples = numSamples, 
  duration = numSamples/fs * 1000, sound = ts(sound, frequency = fs, start=0))
  class(output) = "sound"
  output
}
