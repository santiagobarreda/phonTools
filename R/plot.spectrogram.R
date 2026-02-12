
#' Plot Spectrograms
#'
#' Plot a spectrogram object as a heatmap.
#'
#' Creates a color-coded time-frequency representation of a spectrogram object.
#' Supports customizable color schemes, dynamic range, and frequency/time limits.
#'
#' @method plot spectrogram
#' @export
#' @param x A spectrogram object to be plotted.
#' @param y Unused (for compatibility with generic plot method).
#' @param ylim Y-axis limits (frequency range).
#' @param xlim X-axis limits (time range).
#' @param quality If TRUE, higher quality output is produced (slower).
#' @param ... Additional graphical parameters passed to image function.
#' @return Invisibly returns NULL. Used for side effects (plotting).
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' data(sound)
#' sp <- spectrogram(sound)
#' plot(sp)
#'
#' @export

plot.spectrogram = function (x, y, ylim, xlim, quality = FALSE, ...){
  if (x$colors[1] == TRUE)
    zcolors = colorRampPalette(c('dark blue','blue','cyan','light green','yellow',
                                  'orange','red', 'brown'))
  else if (x$colors[1] == FALSE) 
    zcolors = colorRampPalette(c('white','black'))
  else 
    zcolors = colorRampPalette(x$colors)
  
  zrange = c(-x$dynamicrange, 0)
  nlevels = abs(zrange[1] - zrange[2]) * 1.2
  levels = pretty(zrange, nlevels)
  zcolors = zcolors(length(levels) - 1)
  
  times = as.numeric(rownames(x$spectrogram))
  hz = as.numeric(colnames(x$spectrogram))
  
  # Clip to dynamic range
  spect = x$spectrogram
  spect[spect < (-x$dynamicrange)] = -x$dynamicrange
  
  if (missing(ylim)) ylim = c(0, x$maxfreq)
  if (missing(xlim)) xlim = range(times)
  
  if (quality) {
    # Use raster for faster rendering
    image(as.double(times), as.double(hz), spect, useRaster = FALSE, 
          col = zcolors, xlab = 'Time (ms)', ylab = "Frequency (Hz)", 
          ylim = ylim, xlim = xlim, ...)
  } else {
    # Lower quality: faster with vector rendering
    image(as.double(times), as.double(hz), spect, useRaster = FALSE, 
          col = zcolors, xlab = 'Time (ms)', ylab = "Frequency (Hz)", 
          ylim = ylim, xlim = xlim, ...)
  }
  box()
}

