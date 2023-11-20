# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






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
#' 
vplot = function (x, y, labels = NULL, colors = NULL, points = NULL, meansonly = FALSE, ellipsesd = 0, 
                  add = FALSE, alternateaxes = FALSE, xsampa = FALSE, logaxes = FALSE, ...){
  if (min(table (labels)) < 2 & ellipsesd>0) 
    stop ('At least 3 tokens per category are required to plot ellipses.')
  if (logaxes & min(x,y) <= 0) stop ('Log axes are incompatible with negative plotting values.')
  
  cl = match.call()
  matched = match(c('labels', 'meansonly', 'ellipsesd', 'add', 'colors', 
                    'alternateaxes', 'xsampa','points','logaxes'),names(cl),0)
  p = cl[-matched] ## for plot()
  args = sapply (2:length(p), function(x) p[[x]])
  names(args) = names(p)[-1]
  
  if (alternateaxes){tmp = x; x = y; y = tmp; 
                     tmp = args$x; args$x = args$y; args$y = tmp}
  allx = x; ally = y; alllabels = labels;
  
  if (meansonly) oranges = c(range(x), range(y))
  if (meansonly & !is.null(labels)){ 
    x = tapply (x, labels, mean); args$x = call('(', x);
    y = tapply (y, labels, mean); args$y = call('(', y)
    labels = names(y)
  }
  if (meansonly & is.null(labels)) 
    stop ('Mean vowel category plotting only possible if labels are given.')
  
  if (logaxes & !add) args$log = call('quote', 'xy')
  
  #######
  if (match("xlim", names(args), 0)>0) xlim = eval(args[[match("xlim", names(args), 0)]])
  if (match("ylim", names(args), 0)>0) ylim = eval(args[[match("ylim", names(args), 0)]])
  
  if (match("xlim", names(args), 0)==0 & !logaxes)
    xlim = range(allx)+c(-abs(diff(range(allx)))/20,abs(diff(range(allx)))/20)  
  if (match("ylim", names(args), 0)==0 & !logaxes)
    ylim = range(ally)+c(-abs(diff(range(ally)))/20,abs(diff(range(ally)))/20)
  
  if (match("xlim", names(args), 0)==0 & logaxes)
    xlim = range(allx)*c(.9,1.1)  
  if (match("ylim", names(args), 0)==0 & logaxes)
    ylim = range(ally)*c(.9,1.1)
  
  if (match("xlim", names(args), 0)==0 & !logaxes & meansonly & ellipsesd == 0)
    xlim = range(x)+c(-abs(diff(range(x)))/20,abs(diff(range(x)))/20)  
  if (match("ylim", names(args), 0)==0 & !logaxes & meansonly & ellipsesd == 0)
    ylim = range(y)+c(-abs(diff(range(y)))/20,abs(diff(range(y)))/20)
  
  if (match("xlim", names(args), 0)==0 & logaxes & meansonly & ellipsesd == 0)
    xlim = range(x)*c(.9,1.1)  
  if (match("ylim", names(args), 0)==0 & logaxes & meansonly & ellipsesd == 0)
    ylim = range(y)*c(.9,1.1)  
  
  if (alternateaxes){ xlim=rev(xlim); ylim=rev(ylim);}
  args$xlim = call('c', xlim[1],xlim[2]); args$ylim = call('c', ylim[1],ylim[2]);
  
  if (match("cex.axis", names(args),0)==0) args$cex.axis = call('(', 1.1)
  if (match("cex.lab", names(args),0)==0) args$cex.lab = call('(', 1.1)
  if (match("cex", names(args),0)==0 & meansonly) args$cex = call('(', 3)
  if (match("cex", names(args),0)==0 & !meansonly) args$cex = call('(', 1.2)
  
  if (match("lwd", names(args), 0)==0) lwd = 2
  if (match("lwd", names(args), 0)>0) lwd = args[[match("lwd", names(args), 0)]]
  
  if (match("xlab", names(args),0)==0 & !alternateaxes) args$xlab = call('quote', 'F1 (Hz)')
  if (match("ylab", names(args),0)==0 & !alternateaxes) args$ylab = call('quote', 'F2 (Hz)')
  if (match("xlab", names(args),0)==0 & alternateaxes) args$xlab = call('quote', 'F2 (Hz)')
  if (match("ylab", names(args),0)==0 & alternateaxes) args$ylab = call('quote', 'F1 (Hz)')
  #######
  
  vlevels = levels (as.factor (alllabels))
  vnums = as.numeric (as.factor(alllabels))
  
  if (is.null(colors)) colors = rep(colors()[c(24,506,118,610,30,124,556,258,290,151,84,657,404)],10)
  if (!meansonly) cols = colors[vnums]
  if (!meansonly & length(colors)==length(x)) cols = colors
  if (meansonly) cols = colors
  args$col = call ('[', cols)
  
  if (!is.null(points)){
    points = rep(points, 100)    
    args$pch = call ('(', quote(points[vnums]))
  }
  if (xsampa) args$pch = call ('xsampatoIPA', quote(labels))
  if (!add) do.call ('plot', args)
  if (add) do.call ('points', args)
  
  if (ellipsesd > 0){
    for (i in 1:length(vlevels)){  ##fix density so that you can specify number of points not spacing
      if (!logaxes)sdellipse (cbind (allx[alllabels==vlevels[i]],ally[alllabels==vlevels[i]]), 
                              stdev = ellipsesd, col = colors[i],lwd=lwd) 
      
      if (logaxes){ tmp = sdellipse (log(cbind (allx[alllabels==vlevels[i]],ally[alllabels==vlevels[i]])), 
                                     stdev = ellipsesd, show = F); lines (exp(tmp), col = colors[i],lwd=lwd)}
    }
  }   
}

