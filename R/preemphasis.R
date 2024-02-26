
#' Add Preemphasis
#' 
#' Single-zero preemphasis filter.
#' 
#' 
#' @export
#' @param input Either a numeric vector representing a sequence of samples
#' taken from a sound wave or a sound object created with the loadsound() or
#' makesound() functions.
#' @param cutoff The spectral slope is increased by 6 dB. per octave above this
#' frequency.
#' @param fs The sampling frequency of the sound. If a 'sound' object is
#' passed, this does not need to be specified.
#' @param verify If TRUE, before and after spectra are plotted to allow the
#' user to visually verify the process.
#' @param coeff Optionally, the single coefficient used by the filter may be
#' specified.
#' @return The modified sound is returned. If a 'sound' object another 'sound'
#' object is returned.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references
#' 
#' http://www.fon.hum.uva.nl/praat/manual/Sound__Filter__pre-emphasis____.html
#' @examples
#' 
#' signal = sinusoid (c(100, 200, 400, 800, 1600), fs = 4000, 
#' dur = 100, sum = TRUE)[,7]
#' 
#' preemphasis (signal, verify = TRUE, fs = 4000, cutoff = 50)
#' 
preemphasis = function (input, cutoff = 50, fs = 22050, verify = FALSE, coeff = 0){
  soundout = 0
  tsout = 0
  if (inherits(input,"ts")){
    fs = frequency(input)
    tsout = 1
  }
  if (inherits(input,"sound")) {
    soundout = 1
    oldsound = input
    fs = input$fs
    input = input$sound
  }
  if (coeff == 0) coeff = -exp (-2 * pi * cutoff / fs)
  out = as.numeric (filter (input, c(1, coeff), method = 'convolution', sides = 1))
  out[1] = input[1]
  if (verify == TRUE){
    spectralslice (out, fs = fs)
    spectralslice (input, fs = fs, add = TRUE, col = 3, lty = 'dotted')    
  }
  if (soundout == 1){
    oldsound$sound = out 
    invisible (oldsound)
  }
  else if (tsout == 1){
    out = ts (out, frequency = fs, start = 0)
    invisible (out)
  }
  else invisible (out)
}
