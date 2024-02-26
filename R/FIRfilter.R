
#' Perform Digital Filtering
#' 
#' Finite Impulse Response (FIR) filtering of vectors.
#' 
#' This function performs lowpass, highpass and bandpass filtering using a
#' windowed-sinc filter. Increasing the filter order decreases the transition
#' region between the passband and the stopband. The magnitude of frequencies
#' in the stopband is usually attenuated by about about 45 dB.
#' 
#' If verify is TRUE, a plot is created which allows the user to inspect the
#' performance of the function.
#' 
#' @export
#' @param sound A numeric vector representing a waveform, or a 'sound' object
#' created with the loadsound() or makesound() functions.
#' @param from The low cutoff point for the filter. Frequencies higher than
#' this are not attenuated. Minimum allowed value is 0 Hz.
#' @param to The high cutoff point for the filter. Frequencies lower than this
#' are not attenuated. Maximum allowed value is fs/2 Hz.
#' @param fs The sampling frequency of the sound. If a 'sound' object is
#' passed, this does not need to be specified.
#' @param order The number of taps included in the filter. The order of the
#' filter may not exceed the number of samples that make up the sound.
#' @param verify If TRUE, a plot comparing the spectrum of the input sound is
#' compared the the filtered sound.
#' @param impulse If a filter impulse response is specified, this is used to
#' filter the signal.
#' @param pad If TRUE, the signal is padded with zero so that the original
#' length is maintained.
#' @return If a vector is given, the filtered vector is returned.
#' 
#' If a 'sound' object is given, a sound object containing the filtered sound
#' is returned.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/Sinc_filter
#' 
#' Lyons, R. G. (2004). Understanding Digital Signal Processing (2nd ed.).
#' Prentice Hall.
#' @examples
#' 
#' ## generate random noise
#' #sound = rnorm (5000, 0, 100)
#' 
#' ## implement low, high and band-pass filters
#' #par (mfrow = c(3,1), mar = c(4,4,1,1))
#' #snd1 = FIRfilter (sound, from = 400, fs = 1000, verify = T)
#' #snd2 = FIRfilter (sound, to = 400, fs = 1000, verify = T)
#' #snd3 = FIRfilter (sound, from = 400, to = 100, fs = 1000, verify = T)
#' 
#' ## use filters of different orders (i.e., differing number of taps)
#' #par (mfrow = c(2,1), mar = c(4,4,1,1))
#' #snd1 = FIRfilter (sound, to = 400, fs = 1000, order = 50, verify = T)
#' ## higher order filters lead to narrower transition regions
#' #snd2 = FIRfilter (sound, to = 400, fs = 1000, order = 2000, verify = T)
#' 
FIRfilter = function (sound, from = 0, to = fs/2, fs = 22050, order = 200, 
                      verify = FALSE, impulse = NULL, pad = TRUE){
  soundout = 0; tsout = 0;	
  if (order%%2) order = order +1
  if (inherits(sound, "ts")){
    fs = stats::frequency(sound)
    tsout = 1
  }
  if (inherits(sound,"sound")) {
    soundout = 1
    tmp = sound
    fs = sound$fs
    sound = sound$sound
  }
  if (order > length (sound)) order = length(sound)
  if (order < 4) order = 4
  if (from < 0) stop ('Low cutoff must be above 0 Hz.')
  if (from > fs/2) stop ('High cutoff must be below the Nyquist frequency.')
  sdin = sd (sound)
  
  maxamp = max(sound)
  M = order
  n = seq(0,M-1,1)
  Mi = (M-1) / 2  
  fromrads = (((fs/2)-from) / fs) * pi
  torads = (to / fs) * pi
  fromrads2 = (from / fs) * pi
  
  fromh = (-1)^(n)*2*fromrads*sinc((2*fromrads)*(n-Mi))  ##min freq passed
  toh = 2*torads*sinc(2*torads*(n-Mi))  ##max freq passed
  fromh2 = 2*fromrads2*sinc(2*fromrads2*(n-Mi))  
  
  fromh = fromh * windowfunc(length(fromh), type ='blackman')
  toh = toh * windowfunc(length(toh), type ='blackman')
  fromh2 = fromh2 * windowfunc(length(fromh2), type ='blackman')
  
  if (pad) sound = c(rep(sound[1], order/2), sound, rep(tail(sound,1),order/2))
  if (from!=0 & to==fs/2) output = filter (sound, fromh, method = 'convolution')
  if (from==0 & to!=fs/2) output = filter (sound, toh, method = 'convolution')
  if (from!=0 & to!=fs/2) output = filter (sound, fromh2-toh, method = 'convolution')
  if (!is.null(impulse)) output = filter (sound, impulse, method = 'convolution')
  output = output[!is.na(output)]
  output = output/sd(output) * sdin
  if (pad) output = output[-1]
  
  if (verify == TRUE){
    spectralslice (sound, fs = fs, col = 3, lty = 'dashed', ylim = c(-110,0), padding = 0, window = 'kaiser')  
    spectralslice (output, fs = fs, add = TRUE, padding = 0, window = 'kaiser') 
    abline (v = c(from,to), lwd = 2, col = 2)
  }  
  if (soundout == 1){
    tmp$sound = output
    output = tmp
  }
  else if (tsout == 1){
    output = ts (output, frequency = fs, start = 0)
  }
  invisible (output)
}
