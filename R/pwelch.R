
#' Welch's Power Spectral Density Estimate
#' 
#' Calculates a power spectral density estimate using Welch's method.
#' 
#' This function divides the signal into a number of equally-sized windows,
#' finds the power spectrum of each one, and then finds the average across all
#' windowed sections.
#' 
#' @export
#' @param sound A vector representing a sound wave, or a 'sound' object.
#' @param points The number of points to be included in each window. If not
#' specified, the sound is divided into 10 equally-sized windows.
#' @param overlap The amount of overlap between adjacent segments, in points.
#' @param padding The amount of zero padding to be applied to each window.
#' @param window The window to be applied to each segment of the signal. See
#' windowfunc() for available options.
#' @param show If FALSE, no plot is created.
#' @param fs The sampling frequency of the sound. If a 'sound' object is passed
#' this does not need to be specified.
#' @param preemphasisf Preemphasis of 3 dB per octave is added to frequencies
#' above the specified frequency.
#' @param zeromax If TRUE, the maximum dB is set to 0.
#' @param type The line type to be used for plotting, passes its value to the
#' 'lty' parameter.
#' @param ... any additional arguments will be passed to the internal calls of
#' 'plot'.
#' @return The following columns:
#' 
#' \item{hz}{The center frequency of the analysis bins.} \item{dB}{The dB
#' magnitude/power for the analysis bin.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' ## make a sine wave
#' sinewave = sinusoid (f = 300, fs = 1000, dur = 2000, sum = FALSE)
#' 
#' ## add noise
#' sinewave = sinewave[,2] + rnorm (length (sinewave[,2]), 0, 3)
#' 
#' ## compare the results of pwelch() and spectralslice()
#' par (mfrow = c(2,1), mar = c(4,4,1,1))
#' spectralslice (sinewave, fs = 1000)
#' pwelch (sinewave, points = 400, fs = 1000)
#' 
#' 
pwelch = function (sound, points = 0, overlap = 0, padding = 0, window = 'hamming', 
                   show = TRUE, fs = 1, preemphasisf = 0, zeromax = TRUE, type,...){
  if (inherits(sound,"sound")) {
    fs = sound$fs
    sound = sound$sound
  }
  if (inherits(sound,"ts")) fs = frequency(sound)

  if (preemphasisf > 0) sound = preemphasis (sound, preemphasisf, fs)
  
  n = length (sound)
  if (points == 0) points = ceiling (n / 10)
  
  sound = c(sound, rep(0, points))
  
  spots = seq (1, n, points - overlap)
  
  if ((points+padding) %% 2 == 1) padding = padding + 1
  n = points + padding
  
  magnitude = rep (0, n)
  for (i in 1:length(spots)){
    tmp = sound[spots[i]:(spots[i]+points-1)] * windowfunc(points, type = window)
    tmp = c(tmp, rep (0, padding))
    tmp = fft(tmp)
    tmp = tmp * Conj (tmp)
    magnitude = magnitude + tmp
  }
  magnitude = magnitude / length(spots)
  magnitude = magnitude[1:(n/2+1)]
  magnitude = abs(magnitude)
  dB = log (magnitude, 10) * 20
  if (zeromax == TRUE) dB = dB - max (dB)
  
  if (fs > 1) hz = seq (0, fs/2, length.out = (n/2)+1)
  if (fs == 1) hz = seq (0, .5, length.out = (n/2)+1)
  
  if (missing(type)) type = "l"
  
  if (fs > 1) xlab = 'Frequency (Hz.)'
  if (fs == 1) xlab = 'Frequency / Sampling Freq.'

  if (show == TRUE) plot (hz, dB, type = type, ylab = 'Power (dB.)', xlab = xlab, xaxs = 'i' ,...)  
  
  invisible (cbind (hz, dB))
}

