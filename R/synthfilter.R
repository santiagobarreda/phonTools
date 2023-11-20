# Copyright (c) 2014 Santiago Barreda
# All rights reserved.






#' Filtering by Synthesis
#' 
#' Use Fourier synthesis to recreate signal without undesired frequency
#' components.
#' 
#' This function performs lowpass, highpass and bandpass filtering by
#' performing an FFT on the entire signal, zeroing out coefficients
#' representing undesired frequency components, and performing an IFFT on the
#' result. This approach may not be appropriate for some applications, but it
#' is useful in some cases. This may be slow for long signals.
#' 
#' If verify is TRUE, a plot is created which allows the user to inspect the
#' performance of the function.
#' 
#' @param sound A numeric vector representing a waveform, or a 'sound' object
#' created with the loadsound() or makesound() functions.
#' @param band A vector with exactly two elements, the first specifying the
#' lowest frequency to be passed (must be > 0), and the second specifying the
#' highest frequency to be passed (must be < fs/2).
#' @param fs The sampling frequency of the sound. If a 'sound' object is
#' passed, this does not need to be specified.
#' @param attenuation Attenuation of stop band, in dBs. If left as 0, stopband
#' frequency components are completely omitted.
#' @param verify If TRUE, a plot comparing the spectrum of the input sound is
#' compared the the filtered sound.
#' @return If a vector is given, the filtered vector is returned. If a 'sound'
#' object is given, a sound object containing the filtered sound is returned.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' ## uncomment and run
#' ##  white noise
#' noise = rnorm(5000)
#' 
#' ## low-pass filter
#' #synthfilter (noise, band = c(0, .25), verify = TRUE)
#' 
#' ##  band-pass filter
#' synthfilter (noise, band = c(.15, .25), verify = TRUE)
#' 
#' ##  high-pass filter
#' #synthfilter (noise, band = c(.35, .5), verify = TRUE)
#' 
#' 
synthfilter = function (sound, band = c(0,fs/4), fs = 1, verify = FALSE, attenuation = 0){
  soundout = 0; tsout = 0;	
  if (class(sound) == "ts"){
    fs = frequency(sound)
    tsout = 1
  }
  if (class(sound) == "sound") {
    soundout = 1
    fs = sound$fs
    oldsound = sound
    sound = sound$sound
  } 
  sdin = sd (sound)
  n = length (sound)
  if (attenuation != 0) attenuation = 10^(-abs(attenuation)/10) * 2
  
  freqs = seq (attenuation, fs, length.out = n+1)[-(n+1)]
  pass = rep (attenuation, length(freqs))
  pass[freqs >= band[1] & freqs <= band[2]] = 1
  pass[freqs <= (fs-band[1]) & freqs >= (fs-band[2])] = 1
    
  soundspect = fft (sound)
  output = soundspect * pass                 
  output = Re(fft(output, inverse = TRUE))
  
  if (verify){
    par (mfrow = c(2,1), mar = c(4.5,4.5,3,1))
    spectralslice (sound, fs = fs, padding = 1000, main = 'Input Signal')
    spectralslice (output, fs = fs, padding = 1000, main = 'Output Signal')
  }
  output = output/sd(output) * sdin
  if (soundout == 1)  output = makesound (output, filename = oldsound$filename, fs = fs)
  if (tsout == 1) output = ts (output, frequency = fs, start = 0)
 
  invisible (output)
}

