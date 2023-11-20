# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Frequency Response
#' 
#' Find the frequency response of a digital filter
#' 
#' This function plots (and optionally returns) the frequency response for the
#' digital filter whose transfer function is determined by the numerator and
#' denominator filter coefficients given in b and a.
#' 
#' @param b The moving-average (MA), numerator coefficients of the filter.
#' @param a The autoregressive (AR), denominator coefficients of the filter.
#' Please note that the leading 1 at a[0] is not assumed.
#' @param fs The sampling frequency of the sound. If this is not given
#' calculations are presented as if fs = 1.
#' @param add If TRUE, the frequency response plot is added to an existing
#' plot.
#' @param show If TRUE, the frequency response of the estimated filter is
#' plotted.
#' @param steps The number of steps between zero and the Nyquist frequency.
#' @param ... Additional arguments are passed to internal plotting functions.
#' @return A dataframe with two columns (frequency and response) that can be
#' used to redraw the frequency response if required. The 'response' value
#' corresponds to dB. magnitude below peak.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Lyons, R. G. (2004). Understanding Digital Signal Processing
#' (2nd ed.). Prentice Hall.
#' @examples
#' 
#' 
#' ## make a synthetic vowel with a known set of formant frequencies
#' sound = vowelsynth (ffs = c(500,1500,2500,3500,4500), 
#' fbw = c(30, 80, 150, 200, 220),f0 = 100, dur = 100)
#' 
#' plot (sound)
#' 
#' ## let the LPC function estimate the filter used to generate the vowel
#' coeffs = lpc (sound, show = FALSE)
#' 
#' ## compare frequency response of estimated filter to vowel spectrum
#' spectralslice (sound, col = 4, preemphasisf = 50)
#' freqresponse (1, coeffs, add = TRUE, fs = 10000)
#' 
#' ## generate a sinc function
#' filt = sinc (seq (-15,15,1/2), normalized = TRUE)
#' ## treat it as a low-pass FIR filter and inspect its frequency response
#' freqresponse (filt, 1)
#' 
#' 
freqresponse = function (b, a, fs = 0, add = FALSE, show = TRUE, steps = 1000,...){
  if (fs > 0) stepsize = (2/fs)
  w = seq (0, pi, length.out = steps)
  j = sqrt (as.complex(-1))
  numerator = 0
  denomenator = 0
  
  for (i in 1:length(b)) numerator = numerator + b[i] * (cos((i-1) * w) - sin((i-1) * w) * j)
  if (length(a) > 1)
  for (i in 2:length(a)) denomenator = denomenator - a[i] * (cos((i-1) * w) - sin((i-1) * w) * j)

  denomenator = 1 - denomenator
  
  response = log(abs (numerator / (denomenator)), 10) * 20
  response = response - max (response)
  
  if (add == FALSE & show == TRUE){
    if (fs == 0) plot (w / (pi*2), response, xlab = '(Frequency / Sampling Freq.)', ylab = 'Power (dB)',
                       type = 'l', lwd = 2, xaxs = 'i', ...)
    if (fs > 0) plot (w*((fs/2) / pi), response, xlab = 'Hz', ylab = 'Power (dB)',
                      type = 'l', lwd = 2, xaxs = 'i', ...)
  }
  if (add == TRUE & show == TRUE){
    if (fs == 0) lines (w / (pi*2), response, lwd = 2, ...)
    if (fs > 0) lines (w*((fs/2) / pi), response, lwd = 2, ...)
  }  
  if (fs == 0) out = data.frame (frequency = w / (pi*2), response = response)
  if (fs > 0) out = data.frame (frequency = w*((fs/2) / pi), response = response)
  invisible (out)
}
