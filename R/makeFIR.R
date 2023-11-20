# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Create a Digital Filter
#' 
#' Design a Finite Impulse Response (FIR) Filter.
#' 
#' Designs Type I FIR filters of odd length (even order). If an odd order is
#' provided, 1 is added to the order. Filters are designed using the
#' window-design method. The filter frequency response is defined at
#' evenly-spaced locations determined by the filter order and the sampling
#' frequency. If frequency specifications do not fall exactly on these points,
#' the nearest appropriate location is used. This design method may lead to
#' 'undesirable' behaviour between specified frequencies. This can be minimized
#' by increasing the filter order and selecting an appropriate window function.
#' 
#' @param frequency The frequencies at which the frequency response of the
#' filter will be specified. The first frequency specified must be equal to 0.
#' The final frequency specified is assumed to be equal to fs/2.
#' @param dB The power (in decibels) of the filter at each specified frequency.
#' @param order The order of the signal.
#' @param signal If a signal is provided, it is filtered and returned.
#' @param window The type of window to be applied to the filter impulse
#' response. Uses the windowfunc() function included in this package.
#' @param verify If TRUE, a series of plots are created to allow the user to
#' verify that the filter is appropriate.
#' @param interpolation Determines 'linear' or 'cubic' interpolation between
#' the specified points. Uses the interpolate() function included in this
#' package.
#' @return If output = TRUE, the impulse response of the filter specified by
#' the user is returned.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Lyons, R. G. (2004). Understanding Digital Signal Processing
#' (2nd ed.). Prentice Hall.
#' @examples
#' 
#' 
#' ## specify a filter with an arbitrary response
#' frequency = c(0, 500, 502, 5000, 5002, 7000, 7002, 11025)
#' dB = c(0, 0, -50,  -50, -10,  -10, -70, -70)
#' 
#' ## create the filter and verify that the frequency response is as desired
#' testfilter = makeFIR (frequency, dB, verify = TRUE, order = 1500)
#' spectralslice (testfilter, padding = 1000)
#' 
#' 
#' ## create the filter and verify that the frequency response is as desired
#' makeFIR (frequency, dB, verify = TRUE, order = 300, signal = rnorm (1400))
#' 
#' 
makeFIR = function (frequency, dB, order = 200, signal = NULL, window = 'hann', verify = FALSE, interpolation = 'linear'){
  frequency = sort(frequency)  
  if (order %% 2) order = order + 1
  
  if ((frequency[1]) != 0) stop ('First frequency point must be 0.')
  if (length(dB) != length (frequency)) stop ('Frequenecy and dB specifications of unequal length.')
  if (order < 10) stop ('Filter order must be at least 10 taps.')
  
  n = length (frequency)
  if (n < 2) stop ('At least two points are required to specify the filter.')
  
  dB = dB - max (dB)
  ylim = range (dB); ylim[1] = ylim[1]-20; ylim[2] = ylim[2]+5;
  
  spots = seq (0, tail(frequency,1), length.out = order/2 + 1)
  for (i in 1:length (frequency)) frequency[i] = spots[order (abs(spots - frequency[i]))[1]]
  
  incr = spots[2] - spots[1]  
  dBs = interpolate (dB, frequency, steps = 20, increment = incr, show = F, type = interpolation)[,2]
  l = length (dBs)
  dBs = c(dBs, rev(dBs[-c(1,l)]))
  
  dBs = 10^(dBs/20)
  response = Re(fft (dBs, inverse = TRUE))
  response = c(response[l:1], rev(response[l:1][-l])) 
  response = (response / max(response)) * windowfunc(response, type = window)
  
  if (!is.null(signal)) output = FIRfilter (signal, impulse = response) 
  
  oldpar = par()
  if (verify){
    oldpar = par()
	if(is.null(signal)){
		par (mfrow = c(2,1), mar = c(4.5,4.5,3,1))
		plot (response, main = 'Filter Impulse Response', xlab = 'Tap', 
			  ylab = 'Amplitude', xaxs='i', type = 'b')
		spectralslice (response, fs = max(frequency)*2, padding = 15000, main = 'Filter Frequency Response', 
					   ylim = ylim, window = window)
		points (frequency, dB, cex = 1.3, col = 4, pch = 16)
	}  
    if (verify & !is.null(signal)){
		par (mfrow = c(2,2), mar = c(4.5,4.5,3,1))
		plot (response, main = 'Filter Impulse Response', xlab = 'Tap', 
			  ylab = 'Amplitude', xaxs='i', type = 'b')
		spectralslice (response, fs = max(frequency)*2, padding = 15000, main = 'Filter Frequency Response', 
					   ylim = ylim, window = window)
		points (frequency, dB, cex = 1.3, col = 4, pch = 16)
		spectralslice (signal, fs = max(frequency)*2, padding = 5000, main = 'Pre-Filtering', 
					   ylim = ylim, window = window)
		spectralslice (output, fs = max(frequency)*2, padding = 5000, main = 'Post-Filtering', 
					   ylim = ylim, window = window)
		spectralslice (response, fs = max(frequency)*2, padding = 15000, main = 'Filter Frequency Response', 
					   ylim = ylim, window = window, add = TRUE, lty = 'dotted', col = 4)
		par(mfrow=c(1,1),mar=c(5.1,4.1,4.1,2.1));
	  }
	suppressWarnings (par (oldpar)) 
  } 
  if (is.null(signal)) return (response)
  if (!is.null(signal)) return (signal)
}

