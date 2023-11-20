# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Pick IPA Symbols
#' 
#' Select IPA symbols that you wish to include in a plot.
#' 
#' This is an interactive function that allows the user to select IPA symbols
#' for plotting using a chart. The values returned by this function may only be
#' passed to the 'pch' parameter within plotting functions. At the moment it
#' has only been implemented for vowel sounds.
#' 
#' If a vowels vector is given, the function finds the number of categories in
#' the vector. The user is then prompted to select the IPA symbol corresponding
#' to each category by clicking on the correct location on the plot.
#' 
#' If a vowels vector is not provided, the function allows the user to select
#' any desired number of symbols, and returns these in the same order as
#' indicated by the user.
#' 
#' **There may be issues when exporting figures to PDF using IPA font.
#' Exporting plots directly as images works 'out of the box'**
#' 
#' @aliases pickIPA ipainfo
#' @param vowels An optional vector of vowel labels that you would like to plot
#' using IPA symbols.
#' @param n If no vowel vector is provided, the number of symbols desired.
#' @param xsampa If TRUE, x-sampa versions of the symbols are returned.
#' @param description If TRUE, description of the place, manner and voicing of
#' the symbols are also returned.
#' @param verify If TRUE, the selected symbols are plotted in the order
#' indicated by the user, allowing visual confirmation of the selected symbols.
#' @return A list with the following columns (some of which are optional):
#' 
#' \item{IPA}{The symbol which should be passed to 'pch' to plot IPA symbols.}
#' \item{xsampa}{The xsampa representation of each IPA character.}
#' \item{description}{If description = TRUE, a description of each sound.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references http://en.wikipedia.org/wiki/X-SAMPA
#' @examples
#' 
#' ##uncomment to run
#' #vowels = pickIPA (n = 3)
#' #plot (c(1,2,3), c(1,2,3), pch = vowels)
#' 
#' ## select vowels in the order displayed in the console
#' ## to obtain symbols and descriptions of the vowel categories 
#' ## in the Peterson and Barney data. 
#' 
#' # data (pb52)
#' # tmp = pickIPA (pb52$vowel, description = TRUE, xsampa = TRUE)
#' # tmp
#' 
pickIPA = function (vowels, n = 0, xsampa = FALSE, description = FALSE, verify = TRUE){
  if (missing(vowels) & n == 0) stop ('Please provide either a vowels vector or the number of vowels to select.')
  vector = FALSE
  if (!missing(vowels)) vector = TRUE
  if (vector){
    vowels = as.factor (vowels)
    vtypes = levels(vowels)
    nvowels = ntypes (vowels)
    n = nvowels
  }
  IPA = ipainfo()[c(1,4,2,5)]
  plot (IPA[[2]]$frontness+(IPA[[2]]$rounded*.25), IPA[[2]]$height, pch = IPA[[1]],cex = 2, 
  col = 1+IPA[[2]]$rounded*3, xlab = '',ylab = '', xaxt = 'n', yaxt = 'n',xlim = c(.75,3.5),
  ylim=c(0.75,4.25))
  axis (side = 1, at = c(1.15,2.15,3.15), c('Front','Mid','Back'), cex.axis = 1.3)
  axis (side = 2, at = c(1,2,3,4), c('open','open-mid','close-mid','close'), cex.axis = 1.3)

  selected = rep(0,n)
  for (i in 1:n){ 
    if (vector) cat ('Please select ->  ', vtypes[i], '\n\n')
    if (!vector) cat ('Please select vowel ', i, '\n\n')
    flush.console()
    selected[i] = identify (IPA[[2]]$frontness+(IPA[[2]]$rounded*.25), IPA[[2]]$height,'', n = 1)  
  }

  if (verify == TRUE) plot (1:n, rep(1,n), pch = IPA[[1]][selected], ylab='',yaxt='n', xlab='Selection',cex = 2)
  if (vector == TRUE) selected = selected[as.numeric(vowels)]
  out = list (IPA = IPA[[1]][selected])
  if (xsampa == TRUE) out$xsampa = IPA[[4]][selected]
  if (description == TRUE) out$description = IPA[[3]][selected]
  out
}
