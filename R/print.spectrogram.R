
#' Print Spectrogram Objects
#'
#' Display information about a spectrogram object.
#'
#' Prints a brief message about a spectrogram object and indicates that it
#' should be plotted using plot().
#'
#' @method print spectrogram
#' @export
#' @param x A spectrogram object to be printed.
#' @param ... Unused, for compatibility with generic print method.
#' @return Invisibly returns x. Used for side effects (printing).
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' data(sound)
#' sp <- spectrogram(sound)
#' print(sp)
#'
#' @export

print.spectrogram <-
function (x, ...){
  cat ("\nSpectrogram Object")
  cat ("\nView with plot()\n\n")
}
