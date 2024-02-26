
#' Create Sinusoids
#' 
#' Create and display one or more sinusoids.
#' 
#' A number of sinusoids are generated, and optionally plotted and/or returned.
#' The number of frequencies specified must equal the number of amplitudes and
#' initial phases.
#' 
#' @export
#' @aliases sinusoid sinusoids
#' @param freqs A vector of frequencies, one for each desired sinusoid.
#' @param amps A vector of peak amplitudes, one for each desired sinusoid.
#' @param dur The desired duration of the sinusoids, in milliseconds.
#' @param phases A vector of initial phases, one for each desired sinusoid,
#' expressed in radians.
#' @param fs The desired sampling frequency of the sinusoids.
#' @param sum If TRUE, the sum the generated sinusoids is also found.
#' @param show If TRUE, the generated sinusoids are plotted. If sum is TRUE, a
#' second plot is created to display the sum of the sinusoids.
#' @param colors An optional vector of colors used to plot the individual
#' sinusoids. If the number of colors given is less than the number of
#' frequencies specified, the colors are repeated.
#' @return A dataframe with the following columns:
#' 
#' \item{time}{The time, in milliseconds, at which is sample is taken.}
#' \item{waveN}{A series of columns, each indicating the amplitude of wave N at
#' a given time.} \item{sum}{A column indicating the sum of all of the
#' sinusoids.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' ## several waves, and the sum
#' sum = sinusoid (freqs = c(100,200,300), amps = c(1,3,2), 
#' sum = TRUE, show = TRUE)
#' 
#' ## no sum, different phase shifts
#' nosum = sinusoid (freqs = c(100,200,300), amps = c(1,3,2), 
#' phases = c(pi/2, 0, pi/4), sum = FALSE)
#' 
sinusoid = function (freqs, amps = rep(1, length(freqs)), dur = 50, phases = rep(0, length(freqs)), fs = 10000, sum = FALSE, show = FALSE, colors = NULL){
  if (length (freqs) != length (amps)) stop ('Must specify same number of frequencies and amplitudes.')
  if (length (freqs) != length (phases)) stop ('Must specify same number of frequencies and initial phases.')
  amps = abs(amps)
  t = seq (0, dur/1000, 1/fs)  
  n = length (freqs)
  waves = matrix (0, length (t), n)

  if (is.null(colors)) colors = 1:n
  if (length (colors) < n) colors = rep (colors, 100)
  
  for (i in 1:n) waves[,i] = amps[i] * sin (2 * pi * t * freqs[i] + phases[i]) 
  
  if (sum == TRUE) waves = cbind (waves, rowSums (waves))
  
  if (show == TRUE){
    oldpar = par()
    if (sum == TRUE) par (mfrow = c(2,1))
    plot (t*1000,waves[,1], type = 'l', ylab = 'Amplitude', xlab = 'Time (ms)', ylim = c(-max(amps),max(amps)), lwd = 2,xaxs='i')
    abline (h = 0)
    if (n > 1) for (i in 2:n) lines (t*1000, waves[,i], type = 'l', col = colors[i], lwd = 2)
    if (sum == TRUE){
      plot (t*1000,waves[,1+n], col = 1, lwd = 2, type = 'l',xaxs='i', ylab = 'Amplitude', xlab = 'Time (ms)', 
      ylim = range (waves[,1+n]))
      abline (h = 0)
    }
	suppressWarnings (par (oldpar))
  }
  waves = data.frame (time = t*1000, waves)
  colnames(waves)[2:(n+1)] = paste (rep('wave',n), 1:n, sep='')
  if (sum) colnames(waves)[ncol(waves)] = 'sum'
  invisible (waves)
}


sinusoids = function (freqs, amps = rep(1, length(freqs)), dur = 50, phases = rep(0, length(freqs)), fs = 10000, sum = FALSE, show = FALSE, colors = NULL){
  cl = match.call()
  args = sapply (2:length(cl), function(x) cl[[x]])
  names(args) = names(cl)[-1]
  do.call (sinusoid, args)
}

