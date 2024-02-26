
#' Linear Predictive Coding
#' 
#' Predict autoregressive filter coefficients.
#' 
#' LPC coefficients are estimated using the autocorrelation method. The signal
#' is windowed with a Hanning window prior to analysis.
#' 
#' @export
#' @param sound Either a numeric vector representing a sequence of samples
#' taken from a sound wave or a sound object created with the loadsound() or
#' makesound() functions.
#' @param order The number of LPC coefficients to be estimated. By default
#' there is 2 per kHz below the Nyquist frequency plus 3 extra coefficients.
#' @param fs The sampling frequency in Hz. If a sound object is passed this
#' does not need to be specificed.
#' @param show If TRUE, the frequency response of the estimated filter is
#' plotted.
#' @param add If TRUE, the frequency response plot is added to an existing
#' plot.
#' @param preemph If TRUE, preemphasis of 3 dB per octave is applied to the
#' sound before analysis.
#' @return A vector containing the LPC coefficients is returned.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' ## make a synthetic vowel with a known set of formant frequencies
#' sound = vowelsynth (ffs = c(500,1500,2500,3500,4500), 
#' fbw = c(30, 80, 150, 200, 220),f0 = 100, dur = 250)
#' 
#' ## let the LPC function estimate the filter used to generate the vowel
#' coeffs = lpc (sound, show = TRUE)
#' 
lpc = function (sound, order = round(fs/1000) + 3, fs = 10000, show = FALSE, add = FALSE, preemph = TRUE){
  if (inherits(sound,"ts")) fs = frequency(sound)
  if (inherits(sound,"sound")){
    fs = sound$fs
    sound = sound$sound
  }

  if (!is.numeric(sound)) stop("Input must be numeric.")
  if (preemph == TRUE) sound = preemphasis(sound, fs = fs)
  n = length (sound)              
  sound = sound - mean(sound)
  
  sound = sound * windowfunc (sound)
  sound = c(sound, rep(0, order))              
  
  predictors = t(sapply(seq(1, n, 1), function(x) sound[(x):(x + order)]))  
  y = sound[1:n]           
  r = y %*% predictors              
  
  tmp = c(rev(r), r[-1])
  w = t(sapply(seq(order+1, 2, -1), function(x) tmp[(x):(x+order-1)]))       

  coeffs = -r[2:(order+1)] %*% solve(w)
  coeffs = c(1, coeffs)
  
  if (show == TRUE & add == TRUE) 
    freqresponse(1, coeffs, fs = fs, add = add)
  if (show == TRUE & add == FALSE) {
    spectralslice(sound, fs = fs, col = 4)
    freqresponse(1, coeffs, fs = fs, add = TRUE)
  }
  coeffs
}              

