# Copyright (c) 2015 Santiago Barreda
# All rights reserved.





#' Snip/Zoom
#' 
#' Select a subsection of a sound or spectrogram object.
#' 
#' The input object is plotted and the user must click on two points on the
#' plot. If show = TRUE, this function allows the user to zoom in on a
#' subsection of the object. If the output is assigned to a variable, a new
#' object is created and returned that contains only the data in between the
#' two designated points.
#' 
#' @param object A 'sound' or 'spectrogram' object to be truncated.
#' @param show If TRUE, the selected subsection is displayed, resulting in zoom
#' functionality.
#' @return A 'sound' or 'spectrogram' object which interacts with several
#' functions included in this package.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' data (sound)
#' ## the example below is commented because examples cannot 
#' ## require user interaction. simply uncomment the lines below
#' ## and select two points on the plot. 
#' # clipped = snipsound (sound)
#' # spectrogram (clipped)
#' # spectralslice (clipped)
#' 
snip = function (object, show = TRUE){
  if (!inherits(object,"spectrogram") & !inherits(object,"sound")) 
    stop("Input must be a sound or spectrogram object")
  if (inherits(object,"sound")) {
    time = 1:length(object$sound)/object$fs * 1000
    y = object$sound
    touse = seq(1, length(time), 2)
    plot(time[touse], y[touse], xlab = "Time (ms)", ylab = "Amplitude", 
         type = "l", xaxs = "i")
    times = seq(min(time), max(time), length.out = 1000)
    amps = seq(min(y), max(y), length.out = 100)
    edges = identify(rep(times, length(amps)), rep(amps, 
                                                   each = length(times)), "", n = 2)
    edges = edges%%1000
    edges = sort(times[edges])
    T = 1/object$fs
    start = edges[1]/1000/T
    end = edges[2]/1000/T
    snipped = object$sound[start:end]
    newtime = T * 1000 * (1:length(snipped))
    newsound = makesound(snipped, object$filename, object$fs)
    if (show == TRUE) 
      plot(newtime[seq(1, length(snipped), 2)], snipped[seq(1,length(snipped), 2)], 
      xlab = "Time (ms)", ylab = "Amplitude", type = "l", xaxs = "i")
    output = newsound
  }
  if (inherits(object,"spectrogram")) {
    plot(object)
    times = as.numeric(rownames(object$spectrogram))
    times = seq(head(times, 1), tail(times, 1), length.out = 1000)
    freqs = as.numeric(colnames(object$spectrogram))
    freqs = seq(head(freqs, 1), tail(freqs, 1), length.out = 100)
    edges = identify(rep(times, length(freqs)), rep(freqs, 
                                                    each = length(times)), "", n = 2)
    edges = sort(edges%%1000)
    specttimes = as.numeric(rownames(object$spectrogram))
    object$spectrogram = object$spectrogram[specttimes > times[edges[1]] & specttimes < times[edges[2]], ]
    tmp = as.numeric(rownames(object$spectrogram))
    rownames(object$spectrogram) = tmp - min(tmp)
    if (show == TRUE)   plot(object)
    output = object
  }
  invisible(output)
}

