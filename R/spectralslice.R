# Copyright (c) 2023 Santiago Barreda
# All rights reserved.

#' Spectral Slice
#' 
#' A function to plot the power spectrum of a vector representing a sound wave.
#' 
#' 
#' @aliases spectralslice slice
#' @param sound A vector representing a sound wave, or a 'sound' object.
#' @param padding The amount of zero-padding desired for the analysis, in
#' number of samples.
#' @param fs The sampling frequency of the sound. If a 'sound' object is passed
#' this does not need to be specified.
#' @param show If FALSE, no plot is created.
#' @param add If TRUE, the spectrum is added to an existing plot. If FALSE a
#' new one is created.
#' @param window The window to be applied to the signal, applied by
#' windowfunc(), provided in this package.
#' @param windowparameter The parameter for the window to be applied to the
#' signal, if appropriate. Passed to windowfunc().
#' @param zeromax If TRUE, the maximum dB is set to 0.
#' @param type The line type to be used for plotting, passes its value to the
#' 'lty' parameter.
#' @param preemphasisf Preemphasis of 3 dB per octave is added to frequencies
#' above the specified frequency.
#' @param line If TRUE, a line spectrum is created.
#' @param removeDC If TRUE, the DC component is removed.
#' @param \dots any additional arguments will be passed to the internal calls
#' of 'plot' or 'lines'.
#' @return A dataframe with the following elements is returned:
#' 
#' \item{hz}{The center frequency of the analysis bins.} \item{dB}{The dB
#' magnitude/power for the analysis bin.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' ## synthesize schwa-like vowel 
#' vowel = vowelsynth (ffs = c(500,1500,2500,3500,4500))$sound
#'     
#' ## compare window lengths
#' par (mfrow = c(3,1))
#' spectralslice (vowel[500:550], fs = 10000)
#' spectralslice (vowel[500:1000], fs = 10000)
#'      
#' ## line spectrum
#' spectralslice (vowel[500:600], padding = 0, line = TRUE, fs = 10000)
#' 
#' 
spectralslice = function (sound, padding = length(sound) * 2, fs = 1, show = TRUE, 
    add = FALSE, window = "kaiser", windowparameter = 3, zeromax = TRUE, 
    preemphasisf = 0, type, line = FALSE, removeDC = TRUE, ...){
	
   if (inherits(sound,"ts")) fs = frequency (sound)
   if (inherits(sound,"sound")) {
        fs = sound$fs
        sound = sound$sound
    }
	
    if (preemphasisf > 0) 
        sound = preemphasis(sound, preemphasisf, fs)
    n = length(sound)
    if (removeDC) 
        sound = sound - mean(sound)
    sound = sound * windowfunc(n, window, windowparameter)
    N = n + padding
    if (fs > 1) 
        hz = seq(0, fs, length.out = N + 1)
    if (fs == 1) 
        hz = seq(0, 1, length.out = N + 1)
    hz = hz[hz <= fs/2]
    sound = c(sound, rep(0, padding))
    power = abs(fft(sound))
    power = power[1:length(hz)]/(n/2)
    power = log(power, 10) * 20
    power[which(power == min(power))] = sort(power)[2]
    if (zeromax == TRUE) 
        power = power - max(power)
    if (missing(type)) 
        type = "l"
    if (fs > 1) 
        xlab = "Frequency (Hz)"
    if (fs == 1) 
        xlab = "Frequency / Sampling Freq."
    if (add == FALSE & show == TRUE & line == FALSE) 
        plot(hz, power, ylab = "Power (dB)", xlab = xlab, type = type, 
            xaxs = "i", ...)
    if (add == TRUE & show == TRUE & line == FALSE) 
        lines(hz, power, type = type, ...)
    if (line == TRUE) {
        plot(hz, power, ylab = "Power (dB)", xlab = xlab, type = "p", 
            pch = 16, xaxs = "i", ...)
        segments(hz, rep(-5000, length(hz)), hz, power)
    }
    dB = power
    invisible(cbind(hz, dB))
}


slice = function (sound, padding = length(sound) * 2, fs = 1, show = TRUE, 
    add = FALSE, window = "kaiser", windowparameter = 3, zeromax = TRUE, 
    preemphasisf = 0, type, line = FALSE, removeDC = TRUE, ...){
  cl = match.call()
  args = sapply (2:length(cl), function(x) cl[[x]])
  names(args) = names(cl)[-1]
  do.call (spectralslice, args)
}

