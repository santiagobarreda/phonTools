
#' Low pass filter a signal
#' 
#' Low pass filters a signal using a cascaded butterworth filters.  
#' 
#' @export
#' @param sound A numeric vector containing the signal to be filtered.
#' @param r The cutoff frequency of the filter where r = cutoff/Nyquist frequency (Nyquist=fs/2).
#' @param order The order of the filter to be used.  
#' @param n_passes The number of times the filter is applied to the signal. 
#' @return A numeric vector containing the filtered signal.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' \dontrun{
#' x = rnorm(1000)
#' filtered_x = lowpass (x, 0.5)
#' phonTools::spectralslice (x, ylim = c(-200,20), fs=1, col=4)
#' phonTools::spectralslice (filtered_x, ylim = c(-200,20), add = TRUE, col = 2)
#' abline (v=0.25)
#' }

lowpass = function (sound, cutoff = 0.5, order = 6, n_passes = 4){

  
  b = signal::butter(order, cutoff, type="low")
  
  output_sound = signal::filtfilt(b, sound)
  
  if (n_passes>1){
    for (i in 2:n_passes){
      output_sound = signal::filtfilt(b, output_sound)
    }
  }
  
  output_sound
}
