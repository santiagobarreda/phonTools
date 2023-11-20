# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Find the Peaks
#' 
#' Locate the peaks in a numeric vector.
#' 
#' This function looks for peaks by finding elements whose value is greater
#' than both the elements that surround it. If no peaks are found, a value of
#' zero is returned.
#' 
#' @param x A vector whose peaks are to be located.
#' @param show If TRUE, the vector is plotted and peaks are indicated with red
#' triangles.
#' @return A vector indicating the location (position in the vector) of peaks
#' in the vector.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' data (sound)
#' sound2 = sound$sound[10000:11000] 
#' spectrum = spectralslice (sound2, padding = 0, output = TRUE, show = TRUE)
#' peakfind (spectrum[,2])
#' 
peakfind = function (x, show = TRUE){
    rightbig = (diff(c(x[1]+1,x)) > 0)^2
    leftbig = rev((diff(c(rev(x)[1]+1,rev(x))) > 0)^2)

    peaks = leftbig + rightbig
    npeaks = sum(peaks == 2)
    if (npeaks==0) return (0)
    if (npeaks>0) peaks = which(peaks==2)
    if (show == TRUE & npeaks>0){
        plot(x, lwd = 2, type = "l", ylab = "Value", xaxs = "i")
        points(peaks, x[peaks], col = 2, pch = 17, cex = 1)
    }
    invisible(peaks)
}

