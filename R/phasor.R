
#' Plot Phasors
#' 
#' Plot phasors representing one or more complex-valued variables.
#' 
#' Complex-valued variables may be created using the complex() function.
#' 
#' @export
#' @param num A complex-valued variable to be plotted as a phasor, or a vector
#' of such variables.
#' @param scaled If TRUE, phasor magnitudes are scaled to 1.
#' @param add If TRUE, phasors are plotted on existing figure. If FALSE, a new
#' plot is created.
#' @param circle If TRUE and scaled is TRUE, the unit circle is drawn.
#' @param xlim x-axis range.
#' @param ylim y-axis range.
#' @param ... Additional arguments are passed to the internal call of 'plot'
#' and 'arrows'.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' noise = rnorm (100)
#' phasors = fft(noise)
#' 
#' par (mfrow = c(1,3))
#' phasor (phasors)
#' phasor (phasors, circle = TRUE)
#' phasor (phasors, scaled = FALSE)
#' 
#' 
phasor = function (num, scaled = TRUE, add = FALSE, circle = FALSE, xlim,ylim, ...){
  if (!is.complex(num)) stop ('Input must be complex valued.')

  num = matrix(cbind(Re(num), Im(num)), length(num), 2)

  if (!scaled & missing(xlim)) xlim = max(abs(num[,1]))*c(-1,1) 
  if (!scaled & missing(ylim)) ylim = max(abs(num[,2]))*c(-1,1) 
  for (i in 1:nrow(num)){
    tmpnum = num[i,]
    if (scaled) tmpnum = tmpnum / sqrt(tmpnum[1]^2+tmpnum[2]^2)
    
	if (add & (dev.cur() == 1)) add = FALSE
    if (add | i > 1) arrows (0,0, tmpnum[1], tmpnum[2], ...)
    if (!add & i == 1){
      if (scaled) plot (0,0,type='n',xlim = c(-1.2,1.2), ylim = c(-1.2,1.2),xlab='Real Part', ylab='Imaginary Part')
      if (!scaled) plot (0,0,type='n',xlab='Real Part', ylab='Imaginary Part',xlim=xlim,ylim=ylim, ...)
      arrows (0,0, tmpnum[1], tmpnum[2], ...)
      if (circle) abline (h = 0, v = 0, lty = 'dotted')
      if (circle & scaled) sdellipse (matrix (c(1,0,0,1),2,2), means = c(0,0), stdev = 1)
    }
  }
}


