
#' Convert X-Sampa to IPA
#' 
#' Convert a vector of x-sampa vowel descriptions to values that can be used to
#' plot IPA.
#' 
#' This function converts x-sampa to values that can be used to plot IPA
#' symbols by passing them to the 'pch' parameter within plotting functions. At
#' the moment it has only been implemented for vowel sounds. The function also
#' generate a figure comparing IPA to x-sampa vowel representations that may be
#' useful to some users.
#' 
#' **There may be issues when exporting figures to PDF using IPA font.
#' Exporting plots directly as images works 'out of the box'**
#' 
#' @export
#' @param vowels A vector representing vowel labels in x-sampa to be converted
#' to values that can be used to plot the corresponding IPA symbols. If this is
#' not provided a plot comparing IPA to x-sampa representations is displayed.
#' @param chart If TRUE, a plot comparing IPA to x-sampa representations is
#' displayed.
#' @param verify If TRUE, the selected symbols are plotted to allow for
#' verification.
#' @return A vector of the same length as 'vowels', with the values required to
#' plot the desired vowels.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/X-SAMPA
#' @examples
#' 
#' 
#' ## compare x-sampa and IPA vpwel charts
#' #xsampatoIPA ()
#' ## some examples
#' #IPA = xsampatoIPA (c('I','3','e','Q'))
#' #plot (1:5, pch = IPA, cex = 2)
#' 
#' 
xsampatoIPA = function (vowels, chart = FALSE, verify = FALSE){
  oldpar = par()
  if (missing(vowels) | chart == TRUE){ 
    par (mfrow = c(1,2))
    IPA = ipainfo()[c(1,4,5)]
    plot (IPA[[2]]$frontness+(IPA[[2]]$rounded*.25), IPA[[2]]$height, pch = IPA[[1]],cex = 1.5, 
          col = 1+IPA[[2]]$rounded*3, xlab = '',ylab = '', xaxt = 'n', yaxt = 'n',xlim = c(.75,3.5),ylim=c(0.75,4.25))
    axis (side = 1, at = c(1.15,2.15,3.15), c('Front','Mid','Back'))
    axis (side = 2, at = c(1,2,3,4), c('open','open-mid','close-mid','close'))
  
    plot (IPA[[2]]$frontness+(IPA[[2]]$rounded*.25), IPA[[2]]$height, type = 'n', cex = 1.5, 
          col = 1+IPA[[2]]$rounded*3, xlab = '',ylab = '', xaxt = 'n', yaxt = 'n',xlim = c(.75,3.5),ylim=c(0.75,4.25))
    text (IPA[[2]]$frontness+(IPA[[2]]$rounded*.25), IPA[[2]]$height, label = IPA[[3]],cex = 1.25, 
          col = 1+IPA[[2]]$rounded*3)
    axis (side = 1, at = c(1.15,2.15,3.15), c('Front','Mid','Back'))
    axis (side = 2, at = c(1,2,3,4), c('open','open-mid','close-mid','close'))
	suppressWarnings (par (oldpar))
  }
  if (!missing(vowels)){ 
    IPA = ipainfo()[c(1,5)]
    out = as.hexmode(rep (0, length (vowels)))
    for (i in 1:length(IPA[[2]]))
      if (sum(IPA[[2]][i] == vowels)>0) out[IPA[[2]][i] == vowels] = IPA[[1]][i]
    if (verify == TRUE){
	  par (mfrow = c(1,1))
      plot (1:length(out), rep(1,length(out)), pch = out, ylab='',yaxt='n', xlab='Selection',cex = 2,xaxt = 'n')
	  axis (side = 1, at  = 1:length(out))
      suppressWarnings (par (oldpar))
    }
    return(out)
  }
 
}

