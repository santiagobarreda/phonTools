
#' Resample a Sound
#' 
#' Resample using sinc interpolation.#' 
#' 
#' @export
#' @param sound Either a numeric vector representing a sequence of samples
#' taken from a sound wave or a sound object created with the loadsound() or
#' makesound() functions.
#' @param newfs The new desired sampling frequency.
#' @param oldfs The original sampling frequency. If a 'sound' object is
#' provided, this does not need to be specified.
#' @param precision The number of samples before and after the current point to
#' be used for interpolation.
#' @param filterorder The number of taps to be used for the low-pass Butterworth
#' filters used, where appropriate.
#' @param n_passes The number of passes through the low pass filter.
#' @return The resampled vector is returned. If a 'sound' object is passed, the
#' resampled sound is returned as an object.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' \dontrun{
#' data (sound)
#' # downsample and then upsample the sound back to 
#' # its original sampling frequency
#' downsamped = resample (sound, 11025)
#' upsamped = resample (downsamped, 22050)
#' 
#' # compare a part of the waveforms for all three sounds
#' par (mfcol = c(3,2), mar = c(4,4,1,1))
#' plot (sound$sound[1:14000], type = 'l')
#' plot (downsamped$sound[1:7000], type = 'l', col = 4)
#' plot (upsamped$sound[1:14000], type = 'l', col = 2)
#' 
#' phonTools::spectralslice(sound, xlim = c(0,11025),ylim=c(-90,5))
#' phonTools::spectralslice(downsamped, xlim = c(0,11025),ylim=c(-90,5),col=4)
#' phonTools::spectralslice(upsamped, xlim = c(0,11025),ylim=c(-90,5),col=2)
#' }

 
resample = function (sound, newfs, oldfs, precision = 50, filterorder = 6, n_passes = 4){
  
  soundout = 0; 
  
  if (inherits(sound,"ts")){
    fs = frequency(sound)
    tsout = 1
  } 
  
  if (inherits(sound,"Wave")) 
    sound = phonTools::makesound (sound@left, 
                                  filename = "UntitledWaveObject.wav", 
                                  fs = sound@samp.rate) 
  
  if (inherits(sound,"sound")) {
    soundout = 1
    oldsound = sound
    oldfs = sound$fs
    sound = sound$sound
  } 

  ratio = newfs / oldfs
  if (ratio < 1) 
    sound = lowpass (sound, cutoff = ratio, order=filterorder, n_passes = n_passes)
  
  newtime = seq (1, length(sound)+1, by = 1/ratio)   
  nearest = round (newtime)                              
  offset = newtime - nearest                                 
  
  sound = c(rep(0,precision), sound, rep(0,precision+1))
  y = newtime * 0
  
  for (i in -precision:precision)
    y = y + sound[nearest+precision+i] * sinc(offset - i, normalized = TRUE)
  
  if (ratio > 1) 
    y = lowpass (y, cutoff = oldfs/newfs, order= filterorder, n_passes = n_passes)
  
  sound = y / (max(y) * 1.05) 
  sound = ts (sound, frequency = newfs, start = 0)
  if (soundout == 1)  sound = makesound (sound, filename = oldsound$filename, fs = newfs)
  return (sound)   
}

