

qq = benchmark("a" = {
  sound = tuneR::readWave ("working/untitled.wav")
  ff1 = formanttrack2(snd, show = FALSE)

  },
  "b" = {
    snd = phonTools::loadsound ("working/untitled.wav")
    ff2 = phonTools::formanttrack(snd, show=FALSE, timestep = 2, windowlength = 50)
  },
replications = 10)

qq

par (mfrow = c(1,2), mar = c(4,4,1,1))
sound = tuneR::readWave ("working/untitled.wav")
ff1 = formanttrack2(sound, show = TRUE)

sound = phonTools::loadsound ("working/untitled.wav")
ff2 = phonTools::formanttrack(sound, show=TRUE, timestep = 2, windowlength = 50)



sound = tuneR::readWave ("working/untitled.wav")
sound = phonTools::loadsound ("working/untitled.wav")

sound = phonTools::loadsound ("working/untitled.wav")
ff2 = phonTools::formanttrack(sound, show=TRUE, timestep = 2, windowlength = 50)


timestep = 5
windowlength = 30
formants = 5
cutoff = 5000
minformant = 200
maxbw = 600
fs = 22050
show = TRUE 
periodicity = .5
returnbw = FALSE


#' Carry out LPC for sound
#'
#' @export
#' @param sound A numeric vector representing a waveform, a 'sound' object
#' created with the loadsound() or makesound() functions, or a 'Wave' object read in using the tuneR package.
#' @param timestep How far the analysis window will advance, in milliseconds.
#' If this value is set to zero, the function is applied to the entire signal.
#' @param windowlength The length of the analysis window. Longer windows will
#' result in increased accuracy with decreased time-resolution.
#' @param formants The desired number of formants to be tracked. Depending on
#' the sound and time point, fewer than this many may be found.
#' @param order The number of LPC coefficients to be estimated. 
#' @param cutoff The maximum analysis frequency. May not be higher than the
#' Nyquist frequency.
#' @param minformant Formants below this frequency are rejected.
#' @param maxbw The maximum bandwidth for accepted formants.
#' @param fs The sampling frequency of the sound. If a 'sound' or 'ts' object
#' is passed, this does not need to be specified.
#' @param preemphasis_frequency The frequency at which preemphasis is applied. For no preemphasis, set this to NA.
#' @param show If TRUE, a plot is created which allows the user to visually
#' inspect the process.
#' @param periodicity A value between 0 and 1. Signal sections with corrected
#' ACF values lower than this are not analyzed. Allows voiceless sections to be
#' excluded from analysis.
#' @param returnbw If TRUE, estimated formant bandwidths are returned.
#' @return A matrix in which each row represents a different time point. The first four columns represent the frequencies of F1-F4, and columns 5-8 represent their bandwidths.
#' @examples
#' \dontrun{
#' sound = readWave2("yoursound.wav")
#' trackformants (sound, maxformant = 5300)
#' }

formanttrack2 = 
  function (sound, timestep = 5, windowlength = 50, formants = 4, order = formants*2+3, 
            cutoff = 5000, minformant = 200, maxbw = 600, fs = NA, 
            preemphasis_frequency = 50, show = TRUE, periodicity = .5, 
            returnbw = FALSE){
  
  if (inherits(sound,"ts")) fs = frequency(sound)
  
  if (inherits(sound,"Wave")) 
    sound = phonTools::makesound (sound@left, 
                                  filename = "UntitledWaveObject.wav", 
                                  fs = sound@samp.rate) 
  
  if (inherits(sound,"sound")) {
    fs = sound$fs
    sound = sound$sound
  }
  
  if(is.na(fs)) 
    stop ("Sampling frequency must be specified.")
  if (!is.numeric(sound)) 
    stop ('Sound must be numeric.')
  if (timestep < 0) 
    stop ('Timestep must be positive.')
  if (windowlength < timestep) 
    stop ('Window length must be greater than or equal to timestep.')
  if (cutoff > fs/2) 
    stop ('Cutoff frequency must be less than or equal to the Nyquist frequency.')
  
  if (cutoff != fs/2){
    sound = phonTools::resample (sound, maxformant*2, fs)
    fs = cutoff*2
  }
  
  if (!is.na(preemphasis_frequency)){
    coeff = -exp(-2 * pi * preemphasis_frequency/fs)
    sound = signal::filter(c(1,coeff),1,sound)
  }
  
  sound = sound / stats::sd (sound)
  
  n = length (sound)
  duration = n / fs * 1000 
  times = seq (1000/fs,duration-(windowlength+1), timestep)
  spots = round (times * fs)/1000
  windowlength_pts = round (windowlength * fs / 1000)
  
  # change window type here
  window = phonTools::windowfunc(windowlength_pts, "gaussian")
  snd_matrix = (sapply (spots, function (x) sound[x:(x+windowlength_pts-1)]*window))
  
  lpc_coeffs = sapply (1:ncol(snd_matrix),function(x) 
    stats::ar.burg(snd_matrix[,x], order.max = order, aic = FALSE)$ar
  )
  
  formant_results = sapply (1:ncol(lpc_coeffs),function(x) 
    solvelpc(lpc_coeffs[,x], fs = fs, nreturn = formants)
  )
  ffs = t(formant_results)
  colnames (ffs) = c(paste0("f",1:formants),paste0("b",1:formants))
  
  output = data.frame(time = round(times), ffs)
  
  if (show == TRUE){
    phonTools::spectrogram (sound, colors = FALSE, quality = FALSE, fs = 10000, maxfreq = 5000)
    for (i in 1:formants) points (output[ffs[,i]!=0,1], ffs[ffs[,i]!=0,i], pch = 16, col = i, cex = .75)
  }
  
  return (output)
}



slice(r[,10], fs = 10000)
slice(snd_matrix[,10], fs = 10000, add = TRUE, col = 2)




slice(sound, fs = 10000)

slice(sound2, fs = 10000, add = TRUE, col = 2)
