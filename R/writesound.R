
#' Write out a WAV file
#' 
#' Create a WAV file from a numeric vector or 'sound' object.
#' 
#' This function generates single channel (mono), 16-bit WAV sound files at a
#' desired sampling frequency. If a 'sound' object is passed, the filename and
#' sampling frequency do not need to be set. If a filename is not set, the
#' filename defaults to 'samples.wav' where 'samples' indicates the name of the
#' samples variable that was passed to the function.
#' 
#' @export
#' @param samples A numeric vector representing a sound wave.
#' @param filename A string indicating the desired output file name.
#' @param fs The desired output sampling frequency. If a sound object is passed
#' this does not need to be specified.
#' @param bit The desired bit rate. 
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
#' @examples
#' \dontrun{
#' ## generate a sine wave with a frequency of 1000 Hz
#' ## sampled at a frequency of 10000 Hz
#' x = seq (0,.1, 1/10000)
#' snd = sin (2*pi*1000*x)
#' plot (snd[1:100], type = 'b')
#' 
#' ## write out sine wave as a WAV file
#' writesound (snd, filename = '1khz.wav', fs = 10000)
#' 
#' ## if no filename is provided, this sound will be called 'snd.wav'
#' writesound (snd, fs = 10000)
#' }
#' 
writesound = function (samples, filename = '', fs = 22050, bit = 16){
  if (inherits(samples,"sound")){
    if (filename == '') filename = samples$filename
    fs = samples$fs
    bit = samples$bit
    samples = samples$sound
  }
  if (inherits(samples,"ts")) fs = frequency (samples)
  
  if (!is.numeric(samples)) stop("Non-numeric sample values given.")
  if (filename == '') filename = paste (deparse(substitute(samples)), '.wav', sep='')

  tmp_sound = tuneR::Wave (left = samples, samp.rate = fs, bit = bit)
  tuneR::writeWave(tmp_sound, filename)
}


