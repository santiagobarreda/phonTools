# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Resample a Sound
#' 
#' Resample using sinc interpolation.
#' 
#' 
#' @param sound Either a numeric vector representing a sequence of samples
#' taken from a sound wave or a sound object created with the loadsound() or
#' makesound() functions.
#' @param newfs The new desired sampling frequency.
#' @param oldfs The original sampling frequency. If a 'sound' object is
#' provided, this does not need to be specified.
#' @param precision The number of samples before and after the current point to
#' be used for interpolation.
#' @param filterorder The number of taps to be used for the low-pass FIR
#' filters used, where appropriate.
#' @param synthfilter If TRUE, synthfilter() is used for filtering.
#' @return The resampled vector is returned. If a 'sound' object is passed, the
#' resampled sound is returned as an object.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' data (sound)
#' ## downsample and then upsample the sound back to 
#' ## its original sampling frequency
#' #downsamped = resample (sound, 11025)
#' #upsamped = resample (downsamped, 22050)
#' 
#' # compare a part of the waveforms for all three sounds
#' #par (mfrow = c(3,1), mar = c(4,4,1,1))
#' #plot (sound$sound[1:14000], type = 'l')
#' #plot (upsamped$sound[1:14000], type = 'l', col = 2)
#' #plot (downsamped$sound[1:7000], type = 'l', col = 4)
#' 
resample = function (sound, newfs, oldfs, precision = 50, filterorder = 200, synthfilter = FALSE){
  soundout = 0; tsout = 0;
  if (class(sound) == "ts"){
    fs = frequency(sound)
    tsout = 1
  } 
  if (class(sound) == "sound") {
    soundout = 1
    oldsound = sound
    oldfs = sound$fs
    sound = sound$sound
  } 
  ratio = oldfs / newfs 
  if (ratio > 1 & !synthfilter) sound = FIRfilter (sound, to = newfs/2, fs = oldfs, order = filterorder)
  if (ratio > 1 & synthfilter) sound = synthfilter (sound, band = c(0,newfs/2), fs = oldfs)
  
  newtime = seq (1, length(sound)+1, by = ratio)   
  nearest = round (newtime)                              
  offset = newtime - nearest                                 
  
  sound = c(rep(0,precision), sound, rep(0,precision+1))
  y = newtime * 0
  
  for (i in -precision:precision)
    y = y + sound[nearest+precision+i] * sinc(offset - i, normalized = TRUE)
  
  if (ratio < 1 & !synthfilter) y = FIRfilter (y, to = oldfs/2, fs = newfs, order = filterorder)
  if (ratio < 1 & synthfilter) y = synthfilter (y, band = c(0,oldfs/2), fs = newfs)
  
  sound = y / (max(y) * 1.05) 
  if (soundout == 1)  sound = makesound (sound, filename = oldsound$filename, fs = newfs)
  if (tsout == 1)  sound = ts (sound, frequency = newfs, start = 0)
  return (sound)   
}

