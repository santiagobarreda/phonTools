# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Power tracking
#' 
#' Create a power track for a sound.
#' 
#' Returns the average power in the signal in a section as determined by the
#' parameters of the function. A Hann window is applied to each section prior
#' to analysis. Sections with zero power are ignored and not returned.
#' 
#' @param sound A numeric vector representing a waveform, or a 'sound' object
#' created with the loadsound() or makesound() functions.
#' @param timestep Determines how far the window will be moved for each
#' adjacent analysis, in milliseconds.
#' @param windowlength Determines how much of the signal is included for each
#' analysis step, in milliseconds. If this is too small, pitch-synchronous
#' ripples will be seen in the track.
#' @param fs The sampling frequency of the sound. If a 'sound' or 'ts' object
#' is passed, this does not need to be specified.
#' @param show If TRUE, the track is plotted.
#' @param zeromax If TRUE, the maximum dB value is set to zero.
#' @param ... Additional parameters are passed to the internal call of plot(),
#' and used to create the figure.
#' @return A dataframe with the following columns:
#' 
#' \item{time }{ The time, in milliseconds, of each point of analysis.}
#' \item{power }{ The power, in decibels, at each analysis point.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' ## plot the waveform and power of a given sound.
#' data (sound)
#' # sound = loadsound()        ## run this line to use you own sound
#' 
#' ## compare waveform and power
#' par (mfrow = c(2,1), mar = c(4,4,1,1));
#' plot (sound)
#' powertrack (sound)
#' 
powertrack = function (sound, timestep = 5, windowlength = 30, 
                       fs = 22050, show = TRUE, zeromax = TRUE, ...){
  if (inherits(sound,"ts")) fs = frequency(sound)
  if (inherits(sound,"sound")) {
    fs = sound$fs
    sound = sound$sound
  }  
  if (!is.numeric(sound)) stop ('Sound must be numeric.')
  if (timestep < 0) stop ('Timestep must be positive.')
  if (windowlength < timestep) stop ('Window length must be greater than or equal to timestep.')

  T = 1 / fs
  stepsize = round ((timestep / 1000) / T)
  
  half = round(ceiling (windowlength/1000 * 22050)/ 2)
  
  spots = seq (1+half, length(sound)-half, stepsize)
  power = rep (0, length(spots))
  
  for (i in 1:length(spots)){
    section = sound[(spots[i]-half):(spots[i]+half)] 
    power[i] = mean ((section*windowfunc(section))^2)    
  }
  use = (power != 0)
  spots = spots[use]
  power = power[use]
  
  power = log(power, 10)*10
  if (zeromax) power = power - max(power)
  
  time = spots * (1000/fs)
  tmp = data.frame (time = time, power = power)
  
  if (show == TRUE) plot(tmp$time, tmp$power, xlab = 'Time (ms)', ylab = 'Power (dB)', 
                    type = 'l', ylim = c(min(power)-1, 2), lwd = 2, col = 4, ...) 
  invisible (tmp)
}

