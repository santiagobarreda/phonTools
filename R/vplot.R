
#' Plot Vowels
#' 
#' A flexible function that can create a wide variety of vowel plots (including
#' IPA symbols).
#' 
#' 
#' *** This function replaces the older vowelplot() function, which has been
#' deprecated. ***
#' 
#' A flexible vowel plotting function, including functionality to easily
#' generate vowel plots using IPA symbols. This relies on category labels being
#' specified in x-sampa (the required plotting values for IPA symbols may be
#' selected using the pickIPA() function included in this package).
#' 
#' Default parameter values are set for the plot, but these may all be
#' overridden using the standard plotting parameters.
#' 
#' There may be issues when exporting figures to PDF using IPA font. Exporting
#' plots directly as images works 'out of the box'.
#' 
#' @export
#' @aliases vplot IPA
#' @param x A numeric vector indicating formant frequencies to be plotted on
#' the x axis.
#' @param y A numeric vector indicating formant frequencies to be plotted on
#' the y axis.
#' @param labels A vector with labels for vowels. Must be provided for any
#' category-dependent differences in plotting. If x-sampa labels are given IPA
#' symbols may be plotted.
#' @param colors Colors to use for different categories. If specified this
#' overrides automatic colors. It cycles through the list given if number of
#' colors are less than number of categories.
#' @param points Kinds of points to use determined by 'pch' value. If specified
#' it overrides text labels. IPA symbols may be plotted by finding appropriate
#' values using the pickIPA() function included in this package.
#' @param meansonly If TRUE, only category means are plotted (labels must be
#' provided).
#' @param ellipsesd If a number greater than zero is given, ellipses are drawn
#' enclosing this many standard deviations (one per category as indicated by
#' label vector).
#' @param add If TRUE, vowels are plotted on existing figure. If FALSE, a new
#' one is created.
#' @param logaxes Linear axes are used by default, for log axes set to TRUE.
#' @param alternateaxes If TRUE, the origin in the top right corner of the
#' plot, resulting in a configuration like the IPA vowel quadrilateral if F1
#' and F2 are provided. By default the origin in the bottom left corner.
#' @param xsampa If TRUE, the labels vector given to the function is assumed to
#' be specified in x-sampa and IPA symbols are used to plot using the
#' xsampatoIPA() function included in this package. If this is set to TRUE and
#' the 'labels' input is not in x-sampa, the symbols will be wrong.
#' @param \dots Additional arguments are passed to the internal call of 'plot'.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/X-SAMPA
#' @examples
#' \dontrun{
#' 
#' ## A few examples of some vowel plots. 
#' 
#' ## load the Peterson and Barney data
#' data (pb52)
#' pb52 = pb52[pb52$type=='m',]  ## use only the males
#' 
#' par (mfrow = c(3,2), mar = c(4.2,4.2,1,1))
#' 
#' # standard layout with linear axes
#' vplot (pb52$f1, pb52$f2, pb52$vowel, xsampa = TRUE)
#' 
#' # alternate layout with log axes
#' vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = TRUE, 
#'            alternateaxes = TRUE, xsampa = TRUE)
#' 
#' # category means only 
#' vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = TRUE, 
#'            meansonly = TRUE, xsampa = TRUE, cex = 3)
#' 
#' # category means only with standard deviation ellipses
#' vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = FALSE, 
#'        meansonly = TRUE, ellipsesd = 2, xsampa = TRUE)
#' 
#' # same as above, with alternate axes
#' vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = TRUE, 
#'        meansonly = TRUE, ellipsesd = 2, xsampa = TRUE, 
#' 	   alternateaxes = TRUE)
#' 
#' # individual points with standard deviation ellipses
#' # and alternate axes
#' vplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = TRUE, 
#' 	   meansonly = FALSE, ellipsesd = 2, xsampa = TRUE, 
#' 	   alternateaxes = TRUE)
#' 
#' }
vplot = function (x, y, labels = NULL, colors = NULL, points = NULL, meansonly = FALSE, ellipsesd = 0, 
                  add = FALSE, alternateaxes = FALSE, xsampa = FALSE, logaxes = FALSE, ...){
  if (min(table (labels)) < 2 & ellipsesd > 0) 
    stop ('At least 3 tokens per category are required to plot ellipses.')
  if (logaxes & min(x, y) <= 0) stop ('Log axes are incompatible with negative plotting values.')
  
  # Extract plotting args and separate vplot-specific args
  cl = match.call()
  vplot_args = c('labels', 'meansonly', 'ellipsesd', 'add', 'colors', 
                 'alternateaxes', 'xsampa', 'points', 'logaxes')
  matched = match(vplot_args, names(cl), 0)
  plot_call = cl[-matched]
  
  # Prepare data for plotting
  allx = x; ally = y; alllabels = labels
  
  if (alternateaxes) {
    tmp = x; x = y; y = tmp
  }
  
  # Handle means-only mode
  if (meansonly) {
    if (is.null(labels)) stop('Mean vowel category plotting only possible if labels are given.')
    x = tapply(x, labels, mean)
    y = tapply(y, labels, mean)
    labels = names(y)
  }
  
  # Set up plot args
  plot_args = list(x = x, y = y, type = 'n', ...)
  
  # Calculate limits if not provided
  if (!('xlim' %in% names(plot_args))) {
    data_x = if (meansonly) x else allx
    plot_args$xlim = .get_axis_limits(data_x, logaxes)
  }
  if (!('ylim' %in% names(plot_args))) {
    data_y = if (meansonly) y else ally
    plot_args$ylim = .get_axis_limits(data_y, logaxes)
  }
  
  # Reverse limits for alternate axes
  if (alternateaxes) {
    plot_args$xlim = rev(plot_args$xlim)
    plot_args$ylim = rev(plot_args$ylim)
  }
  
  # Set default labels if not provided
  if (!('xlab' %in% names(plot_args))) {
    plot_args$xlab = if (alternateaxes) 'F2 (Hz)' else 'F1 (Hz)'
  }
  if (!('ylab' %in% names(plot_args))) {
    plot_args$ylab = if (alternateaxes) 'F1 (Hz)' else 'F2 (Hz)'
  }
  
  # Set default cex if not provided
  if (!('cex' %in% names(plot_args))) {
    plot_args$cex = if (meansonly) 3 else 1.2
  }
  
  # Set log scale if needed
  if (logaxes & !add) plot_args$log = 'xy'
  
  # Create or add to plot
  if (!add) do.call('plot', plot_args)
  
  # Set up colors and points
  vlevels = levels(as.factor(alllabels))
  vnums = as.numeric(as.factor(alllabels))
  
  if (is.null(colors)) colors = rep(colors()[.default_colors], 10)
  
  # Determine which colors to use
  if (meansonly) {
    cols = colors[1:length(vlevels)]
  } else {
    if (length(colors) == length(x)) {
      cols = colors
    } else {
      cols = colors[vnums]
    }
  }
  
  # Plot points/text
  if (is.null(points)) {
    if (xsampa) {
      points(x, y, pch = xsampatoIPA(labels), col = cols, ...)
    } else {
      text(x, y, label = labels, col = cols, ...)
    }
  } else {
    pch_vals = rep(points, length.out = length(x))
    if (meansonly) pch_vals = pch_vals[1:length(x)]
    points(x, y, pch = pch_vals, col = cols, ...)
  }
  
  # Draw ellipses if requested
  if (ellipsesd > 0) {
    lwd = plot_args$lwd %||% 2
    .draw_ellipses(allx, ally, alllabels, vlevels, ellipsesd, colors, lwd, logaxes)
  }
}

# Internal helper: Calculate axis limits with margin
.get_axis_limits <- function(data, log_scale = FALSE) {
  r = range(data)
  if (log_scale) {
    return(r * c(.9, 1.1))
  } else {
    margin = abs(diff(r)) / 20
    return(r + c(-margin, margin))
  }
}

# Helper: Draw ellipse(s) for categories
.draw_ellipses <- function(x, y, labels, vlevels, ellipsesd, colors, lwd, logaxes) {
  for (i in 1:length(vlevels)){
    data = cbind(x[labels == vlevels[i]], y[labels == vlevels[i]])
    if (!logaxes) {
      sdellipse(data, stdev = ellipsesd, col = colors[i], lwd = lwd)
    } else {
      tmp = sdellipse(log(data), stdev = ellipsesd, show = FALSE)
      lines(exp(tmp), col = colors[i], lwd = lwd)
    }
  }
}

# Default color palette for vowel plots
.default_colors <- c(24,506,118,610,30,124,556,258,290,151,84,657,404)

