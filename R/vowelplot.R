
#' Plot Vowels
#' 
#' A flexible function that can create a wide variety of vowel plots (including
#' IPA symbols).
#' 
#' *** This function has been deprecated and is no longer being developed. I
#' recommend using vplot() instead, which is more flexible and gives the user
#' more control over plotting. ***
#' 
#' This function now includes functionality to easily generate vowel plots
#' using IPA symbols. This relies on category labels being specified in
#' x-sampa. Alternatively, the required plotting values for IPA symbols may be
#' selected using the pickIPA() function included in this package, and then
#' passed to the 'pointType' parameter.
#' 
#' There may be issues when exporting figures to PDF using IPA font. Exporting
#' plots directly as images works 'out of the box'.
#' 
#' @export
#' @param f1s A numeric vector indicating vowel F1 frequencies.
#' @param f2s A numeric vector indicating vowel F2 frequencies.
#' @param labels A vector with labels for vowels. Must be provided for any
#' category-dependent differences in plotting. If x-sampa labels are given IPA
#' symbols may be plotted.
#' @param xrange Allows the user to set the x axis range for the plot.
#' @param yrange Allows the user to set the y axis range for the plot.
#' @param meansOnly If TRUE, only category means are plotted (labels must be
#' provided).
#' @param ellipses If TRUE, standard deviation ellipses are drawn (one per
#' category as indicated by label vector).
#' @param ellipsesd A number indicating the number of standard deviations
#' ellipses will enclose.
#' @param add If TRUE, vowels are plotted on existing figure. If FALSE, a new
#' one is created.
#' @param pointType Kinds of points to use determined by 'pch' value. If
#' specified it overrides text labels. IPA symbols may be plotted by finding
#' appropriate values using the pickIPA() function included in this package.
#' @param colors Colors to use for different categories. If specified this
#' overrides automatic colors. It cycles through the list given if number of
#' colors are less than number of categories.
#' @param logaxes Linear axes are used by default. For log axes set to 'xy'.
#' @param defaultPlot If TRUE, the function plots using pre-determined values.
#' If FALSE, the user has almost complete control over the internal call of
#' 'plot'.
#' @param alternateAxes If TRUE, F1 is plotted on the y axis and F2 on the x
#' axis with the origin in the top right corner. By default F1 is plotted on
#' the x axis and F2 on the y axis with the origin in the bottom left corner.
#' @param xsampa If TRUE, the labels vector given to the function is assumed to
#' be specified in x-sampa and IPA symbols are used to plot using the
#' xsampatoIPA() function included in this package. If this is set to TRUE and
#' the 'labels' input is not in x-sampa, the symbols will be wrong.
#' @param \dots Additional arguments are passed to the internal call of 'plot'.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/X-SAMPA
#' @examples
#' 
#' ## A few examples of some vowel plots. 
#' 
#' data (pb52)
#' par (mfrow = c(1,4), mar = c(4.2,4.2,1,1))
#' 
#' # standard layout with linear axes
#' vowelplot (pb52$f1, pb52$f2, pb52$vowel, xsampa = TRUE)
#' 
#' # alternate layout with log axes
#' vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', 
#' alternateAxes = TRUE, xsampa = TRUE)
#' 
#' # category means only 
#' vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', 
#' meansOnly = TRUE, xsampa = TRUE)
#' 
#' # category means only with standard deviation ellipses
#' vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', meansOnly = TRUE,
#'            ellipses = TRUE, xsampa = TRUE)
#' 
vowelplot = function (f1s, f2s, labels = 0, xrange = NULL, yrange = NULL, 
meansOnly = FALSE, ellipses = FALSE, ellipsesd = 1.96, add = FALSE, pointType = 0, 
colors = NULL, logaxes = '', defaultPlot = TRUE, alternateAxes = FALSE, xsampa = FALSE, ...){
  
  if (is.null(labels)) return (print ('Error: NULL vowel labels given.', quote = FALSE))
  if (is.null(f1s)) return (print ('Error: NULL F1 values given.', quote = FALSE))
  if (is.null(f2s)) return (print ('Error: NULL F2 values given.', quote = FALSE))
  
  labels = as.factor (labels)    ## labels for vowels
  vlevels = levels (labels)      ## levels of vowel categories  
  vnums = as.numeric (labels)    ## a number for each vowel category
  
  ## specially selected colors to make charts easier to read.
  if (is.null(colors)) colors = colors()[c(24,506,118,610,30,124,556,258,290,151,84,657,404)]
  ##colors = hcl(h = seq(0, 360,by = 360 / length (vlevels)), l = 30, c = 90, alpha = 1)  
  
  # cycles through colors and points if less of these than vowel categories
  if (length (colors) < length (vlevels)) colors = rep (colors, 100)
  if (length (pointType) < length (vlevels)) pointType = rep (pointType, 100)
  
  ## if ranges arent set, automatically set them at 5% greater/less than vector ranges
  #if (is.null(xrange) & (meansOnly == FALSE | ellipses == TRUE)) xrange = range(f1s)
  #if (is.null(yrange) & (meansOnly == FALSE | ellipses == TRUE)) yrange = range(f2s)
  #if (is.null(xrange) & meansOnly == TRUE & ellipses == FALSE) xrange = range(tapply (f1s, labels, mean))
  #if (is.null(yrange) & meansOnly == TRUE & ellipses == FALSE) yrange = range(tapply (f2s, labels, mean))
  if (is.null(xrange)) xrange = range(f1s)
  if (is.null(yrange)) yrange = range(f2s)
 
  if (alternateAxes == TRUE){ temp = f1s; f1s = f2s; f2s = temp;}
  
  # calls initial blank plot function  
  if (add == FALSE & defaultPlot == TRUE & alternateAxes == FALSE) 
    plot (0.1,0.1, type = 'n', xlim = xrange, ylim = yrange, xlab = 'F1', ylab = 'F2', 
    cex.lab=1.2, cex.axis=1.2, log = logaxes,...)
  if (add == FALSE & defaultPlot == FALSE & alternateAxes == FALSE) 
    plot (0.1,0.1, type = 'n', log = logaxes, xlim = xrange, ylim = yrange, ...)
  if (add == FALSE & defaultPlot == TRUE & alternateAxes == TRUE) 
    plot (0.1,0.1, type = 'n', xlim = rev(yrange), ylim = rev(xrange), xlab = 'F2', ylab = 'F1', 
    cex.lab=1.2, cex.axis=1.2, log = logaxes,...)

  if (meansOnly == FALSE){  ## if individual vowels are to be plotted
    if (labels[1] != 0){ 
      if (pointType[1] == 0 & xsampa == FALSE) text (f1s, f2s, label = labels, col = colors[vnums], ...)   ## plot text if no pointType specified
      if (xsampa == TRUE) points (f1s, f2s, bg = colors[vnums], col = colors[vnums], pch = xsampatoIPA(labels), ...) ## plot points if so
      if (pointType[1] != 0){
        if (length(pointType) < length(f1s)) points (f1s, f2s, bg = colors[vnums], col = colors[vnums], pch = pointType[vnums], ...) ## plot points if so
        if (length(pointType) == length(f1s)) points (f1s, f2s, bg = colors[vnums], col = colors[vnums], pch = pointType, ...) ## plot points if so
      }
    }
    if (labels[1] == 0){  
      if (pointType[vnums] == 0) pointType[vnums] = 1  ## if no labels
      points (f1s, f2s, type = 'p', pch = pointType[vnums], ...)
    } 
  }
  if (meansOnly == TRUE){   
    if (labels[1] != 0){  
      f1means = tapply (f1s, labels, mean)
      f2means = tapply (f2s, labels, mean)
      if (pointType[1] == 0 & xsampa == FALSE) text (f1means, f2means, label = vlevels, col = colors[1:length(vlevels)], cex = 2)   
      if (xsampa == TRUE) points (f1means, f2means, col = colors[1:length(vlevels)], pch = xsampatoIPA(levels(labels)), ...) ## plot points if so
      if (pointType[1] != 0) points (f1means, f2means, bg = colors[as.numeric(as.factor(labels))], pch = pointType[as.numeric(as.factor(labels))],...)
    }
    if (labels[1] == 0) return (print ('Error: Mean vowel category plotting only possible if labels are given.', quote = FALSE))
  }
  
  if (ellipses == TRUE){
    for (i in 1:length(levels(as.factor(labels)))){
      tempf1s = f1s[levels(as.factor(labels))[i] == labels]
      tempf2s = f2s[levels(as.factor(labels))[i] == labels]
      prcs = prcomp (cbind (tempf1s, tempf2s))   ## linear principal components
      
      if (logaxes == 'xy'){
        tempf1s = log(f1s[levels(as.factor(labels))[i] == labels])
        tempf2s = log(f2s[levels(as.factor(labels))[i] == labels])
        prcs = prcomp (cbind (tempf1s, tempf2s))     ## log principal components
      }
      
      xscale = prcs$sdev[1] * ellipsesd 
      yscale = prcs$sdev[2] * ellipsesd 
      rotatedx = mean(tempf1s) + xscale*cos(seq (0,7,.1))*prcs$rotation[1,1] - yscale*sin(seq (0,7,.1))*prcs$rotation[2,1] 
      rotatedy = mean(tempf2s) + xscale*cos(seq (0,7,.1))*prcs$rotation[2,1] + yscale*sin(seq (0,7,.1))*prcs$rotation[1,1]  
      
      if (logaxes == '')  lines (rotatedx, rotatedy, col = colors[i], lwd = 2)
      if (logaxes == 'xy')  lines (exp(rotatedx), exp(rotatedy), col = colors[i], lwd = 2)
    }
  }
}
