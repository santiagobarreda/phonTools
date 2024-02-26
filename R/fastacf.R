
#' Fast Autocorrelation
#' 
#' Compute the ACF of a signal.
#' 
#' The autocorrelation function is calculated using the inverse Fourier
#' transform applied to the power spectrum. This leads to much faster
#' calculation times than the acf() function included in the typical R
#' installation. Corrections for window type and length are carried out as
#' described in Boersma (1993).
#' 
#' @export
#' @param signal The signal, a numeric vector.
#' @param lag.max The maximum lag value to be returned.
#' @param window The type of window to be applied to the signal. Uses the
#' windowfunc() function in this package. For no window select 'rectangular'.
#' @param show If TRUE, the results are plotted.
#' @param correct If TRUE, the output is corrected based on the window length
#' and the window function that is applied.
#' @return A dataframe with the following columns:
#' 
#' \item{lag }{ Indicates the lag value.} \item{acf }{ Indicates the ACF value
#' at the lag.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Boersma, P., (1993). Accurate short-term analysis of the
#' fundamental frequency and the harmonics-to-noise ratio of a sampled sound.
#' Proc. Instit. Phon. Sci. 17: 97-110.
#' @examples
#' 
#' 
#' ## Uncomment and run the code below to see the speed advantage.
#' ## Raising the n makes the difference even more pronounced.
#' #n = 25000
#' #system.time ({
#' #acf (rnorm (n), plot = F, lag.max = n)
#' #})
#' 
#' #system.time ({
#' #fastacf (rnorm (n), plot = F, lag.max = n)
#' #})
#' 
#' 
fastacf = function (signal, lag.max = length(signal), window = 'hann',
                    show = TRUE, correct = FALSE){
  if (inherits(signal,'sound')) signal = signal$sound
 
  n = length (signal)
  if (lag.max > n) lag.max = n 
  
  signal = signal - mean (signal)
  signal = c(signal*windowfunc(signal, type = window), zeros (signal))
  
  pad=0
  #if (pad){  
  #  pad = 2^ceiling (log (length (signal),2)) - length (signal)
  #  signal = c(signal, zeros (pad))
  #}
  s = fft (signal)
  s = Re (fft (s * Conj(s), inverse = TRUE))
  s = s[1:(lag.max)] / s[1]
  
  if (correct){
    w = fft (c(windowfunc (n, type = window), zeros(n), zeros(pad)))
    w = Re (fft (w * Conj(w), inverse = TRUE))
    w = w[1:(lag.max)] / w[1]
    s = s/w
  }
  lags = (0:(lag.max-1))
  
  if (show){
    plot (lags, s, ylim = c(-1,1), xlab = 'Lag', ylab = 'ACF', type = 'l',xaxs = 'i')
    if (correct) abline (v = n - 50, col = 2)
    abline (h = 0, lty = 'dotted')
  }  
  invisible (data.frame (lag = lags, acf = s))
}

