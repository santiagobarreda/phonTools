
#' Plot Sound Objects
#'
#' Plot the waveform of a sound object.
#'
#' Creates a time-domain plot of a sound object's waveform. If the sound is
#' stored as a time series (ts) object, R's built-in plotting is used.
#' Otherwise, time is computed from the sampling frequency.
#'
#' @method plot sound
#' @export
#' @param x A sound object to be plotted.
#' @param ... Additional graphical parameters passed to the plot function.
#' @return Invisibly returns NULL. Used for side effects (plotting).
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' data(sound)
#' plot(sound)
#'
#' @export

plot.sound = function (x, ...){
  if (!exists("xlab")) xlab = 'Time (s)'
  if (!exists("ylab")) ylab = 'Amplitude'

  if (!inherits(x$sound,'ts')) plot ((1:length(x$sound))/x$fs, x$sound, xlab=xlab, 
                                   ylab=ylab, xaxs = 'i', type = 'l', ...)
  if (inherits(x$sound,'ts')) plot (x$sound, xlab=xlab, ylab=ylab, xaxs = 'i', ...)
}
