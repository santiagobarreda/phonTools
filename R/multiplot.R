# Copyright (c) 2015 Santiago Barreda
# All rights reserved.





#' Plot with variable panel sizes
#' 
#' Create plots with columns or rows of unequal sizes.
#' 
#' This function is essentially a wrapper for the layout() function, which
#' allows the user to create multi-panel figures in which each row or column is
#' of a varying height/width.
#' 
#' Please note that small rows or columns might result in an error related to
#' figure margins being too large when you try to create a plot. These may be
#' reduced with the 'mar' parameter for the par() function, which sometimes
#' solves the problem.
#' 
#' @param n The number of figure panels to be created.
#' @param type If 'r', the panels will be arranged in rows, if 'c', the panels
#' will be arranged in columns.
#' @param sizes A vector indicating the percentage of the total width/height
#' taken up by each row/column.
#' @param show If TRUE, the resulting panel layout is shown to the user.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' ## uncomment and run
#' 
#' data (sound)
#' ## run this instead to select your own sound for this demo.
#' # sound = loadsound ()       
#' 
#' #par (mar = c(4,4,1,1))
#' #multiplot (n = 3, sizes = c(.25, .5, .25))  
#' 
#' #plot (sound)
#' #spectrogram (sound, dynamicrange = 50, maxfreq = 7000)
#' #spectralslice (sound)
#' 
#' 
multiplot = function (n = 2, type = 'r', sizes = rep (1/n, n), show = FALSE){
  if (n < 2) stop ('Must specify at least two panels.')
    if (length(sizes) != n) stop ('Incorrect number of panel sizes specified.')
  sizes = sizes / sum(sizes)
  if (type == 'r') layout (mat = matrix(1:n, n), heights = sizes)
  else if (type == 'c') layout (mat = matrix(1:n, 1,n), widths = sizes)
  else stop ('Invalid type. Enter r for by rows and c for by columns.')
  if (show == TRUE) layout.show(n)
}

