
#' Select Slices
#' 
#' Select spectral slices from a spectrogram.
#' 
#' This function allows the user to select a given number of time points on a
#' spectrogram, and to retrieve spectral slices at those time points. The
#' spectrogram object is plotted, and the user must click on the spectrogram a
#' given number of times at the desired points. The analysis parameters of the
#' slices will reflect the parameters used when creating the spectrogram.
#' Setting the 'plot' parameter to FALSE is useful if you want one set of
#' analysis parameters for the visually presented spectrogram and another set
#' for the spectral slices.
#' 
#' @export
#' @param specobject A 'spectrogram' object created with the spectrogram()
#' function included in this package.
#' @param n The number of desired slices.
#' @param plot If FALSE, the spectrogram is not plotted.
#' @param ... Additional arguments are passed to the internal call of
#' plot.spectrogram().
#' @return A dataframe with one column for each spectral slice selected. Row
#' names indicate frequencies, column names indicate times in milliseconds.
#' Values are decibels below peak.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' ## uncomment to run
#' 
#' # sound = vowelsynth ()
#' # spect = spectrogram (sound)
#' # slices = selectslice (spect, n = 3)
#' 
#' 
selectslice = function (specobject, n = 1, plot = TRUE,...){
  if (!inherits(specobject,'spectrogram')) stop ('Spectrogram object must be provided.')
  if (n < 1 | n %% 1 > 0) stop ('Positive integer n only.')

  if (plot) plot (specobject, ...)
  spect = specobject$spectrogram
  freqs1 = as.numeric(colnames(spect))
  times1 = as.numeric(rownames(spect))

  times = rep(times1, length(freqs1))
  freqs = rep(freqs1, each = length(times1))

  tmp = identify(times, freqs, "", n = n)
  time = sort(times[tmp])

  slices = NULL
  for (i in 1:n) slices = cbind (slices, spect[rownames(spect) == time[i],])
  colnames (slices) = time
  slices
}
