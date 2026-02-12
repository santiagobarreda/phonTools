
#' Print Sound Objects
#'
#' Display information about a sound object.
#'
#' Prints summary information about a sound object including the filename,
#' sampling frequency, duration, and number of samples.
#'
#' @method print sound
#' @export
#' @param x A sound object to be printed.
#' @param ... Unused, for compatibility with generic print method.
#' @return Invisibly returns x. Used for side effects (printing).
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' data(sound)
#' print(sound)
#'
#' @export

print.sound = function (x, ...){
  cat ("\n      Sound Object\n")
  cat ("\n   Read from file:        ", x$filename)
  cat ("\n   Sampling frequency:    ", x$fs, ' Hz')
  cat ("\n   Duration:              ", x$duration,  ' ms')
  cat ("\n   Number of Samples:     ", x$numSamples, '\n')
  cat ("\n")
}
