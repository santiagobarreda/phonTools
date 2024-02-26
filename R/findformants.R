
#' Find Formants
#' 
#' Find formants given a sound or set of LPC coefficients.
#' 
#' Formant frequencies are found analytically using the formulas provided in
#' Snell (1993). If Verify = TRUE, the estimated frequency response, formant
#' locations, and a pole-zero plot of the estimated filter are presented.
#' Accepted formants are presented in 5 colors (which are reused if there are
#' more than 5 formants), while rejected formants are presented in black.
#' 
#' @export
#' @param sound A numeric vector representing a waveform, or a 'sound' object
#' created with the loadsound() or makesound() functions.
#' @param fs The sampling frequency of the sound. If a 'sound' object is passed
#' this does not need to be specified.
#' @param coeffs If a number is given, this many coefficients are used in the
#' analysis. Alternatively, the LPC (AR filter) coefficients may be passed to
#' the function directly using this parameter. For good results, two
#' coefficients are required for each formants plus 2 or 3 'for the pot'.
#' @param maxbw The maximum bandwidth for accepted formants.
#' @param minformant Formants below this frequency are rejected.
#' @param verify If TRUE, a plot is created which allows the user to visually
#' inspect the process.
#' @param showbws If TRUE, formant bandwidths are indicated on the plot.
#' @param showrejected If TRUE, rejected formant locations are indicated.
#' @return A dataframe with the following elements is returned:
#' 
#' \item{frequency}{The frequencies of formants, in ascending order.}
#' \item{bandwidth}{The corresponding formant bandwidth.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Snell, R.(1993). "Formant location from LPC analysis data", IEEE
#' Transactions on Speech and Audio Processing, 1(2), pp. 129-134.
#' @examples
#' 
#' ## make a synthetic vowel with a known set of formant frequencies
#' ## and bandwidths
#' sound = vowelsynth (ffs = c(500,1500,2500,3500,4500),
#'                     fbw = c(30, 90, 150, 210, 270), f0 = 100)
#' 
#' ## compare different plotting options					
#' findformants (sound)
#' #findformants (sound, showrejected = FALSE)
#' #findformants (sound, showbws = TRUE)
#' 
findformants = function (sound, fs = 10000, coeffs = NULL, maxbw = 600, 
minformant = 200, verify = TRUE, showbws = FALSE, showrejected = TRUE){
  if (missing (sound)) sound = 1

  if (inherits(sound,"ts")) fs = frequency(sound)
  if (inherits(sound,"sound")) {
    fs = sound$fs
    sound = sound$sound
  } 

  if (is.null(coeffs)) coeffs = lpc (sound, fs = fs)
  if (length(coeffs) == 1) coeffs = lpc (sound, fs = fs, order = coeffs)
  
  roots = polyroot (rev(coeffs))
  angs = atan2 (Im(roots), Re(roots))
  formants = round (angs * (fs/(2*pi)), 2)
  nums = order (formants)
  formants = formants[nums]
  bws = -(fs/pi) * log (abs(roots[nums]))
  touse = (bws < maxbw & formants > minformant & formants < fs/2)
  out = data.frame (formant = formants[touse], bandwidth = bws[touse])
  
  if (verify == TRUE){
    multiplot (sizes = c(.7,.3), type = 'c', show = FALSE)
    cols = rep (2:6, 10)
    freqresponse (1, coeffs, fs = fs)
    if (length(sound) > 1) spectralslice (preemphasis(sound,fs=fs), fs = fs, add = TRUE, padding = 0, col = 1, lty = 'dotted')
    for (i in 1:nrow(out)){
      abline (v = out[i,1], lwd = 2, col = cols[i])
      if (showbws == TRUE) abline (v = out[i,1] + out[i,2], lty = 'dotted', col = cols[i])
      if (showbws == TRUE) abline (v = out[i,1] - out[i,2], lty = 'dotted', col = cols[i])
    }    
    if (showrejected == TRUE) abline (v = formants[!touse], lty = 'dotted', lwd = 2)
    plot (roots[nums], xlim = range (-1.1,1.1), ylim = range (-1.1,1.1), pch = 4, lwd = 2, 
          xlab = 'Real', ylab = 'Imaginary', col = !touse)
    sdellipse (means = c(0,0), points = matrix (c(1,0,0,1),2,2), stdev = 1, density = .01)
    abline (h = 0, v = 0, lty = 'dotted')
    tmp = 0
    for (i in 1:length(touse))  
      if (touse[i]){
        tmp = tmp + 1
        points (roots[nums][i], pch = 4, lwd = 2, col = cols[tmp])
      }    
  }
  invisible (out)
}

