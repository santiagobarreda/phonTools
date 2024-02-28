
#' Formant Tracking
#' 
#' Create a formant track for a sound.
#' 
#' This function works by repeatedly calling findformants(), and periodicity is
#' established using pitchtrack(), both of which are included in this package.
#' When no formants are found, or if less than the desired number of formants
#' are found, a value of zero is returned for that formant, at that time point.
#' 
#' Tracked formants are presented over a greyscale spectrogram if show is TRUE.
#' When plotting, different colors are used for each formant to allow the user
#' to distinguish these.
#' 
#' @export
#' @param sound A numeric vector representing a waveform, or a 'sound' object
#' created with the loadsound() or makesound() functions.
#' @param timestep How far the analysis window will advance, in milliseconds.
#' If this value is set to zero, the function is applied to the entire signal.
#' @param windowlength The length of the analysis window. Longer windows will
#' result in increased accuracy with decreased time-resolution.
#' @param formants The desired number of formants to be tracked. Depending on
#' the sound and time point, fewer than this many may be found.
#' @param cutoff The maximum analysis frequency. May not be higher than the
#' Nyquist frequency.
#' @param minformant Formants below this frequency are rejected.
#' @param maxbw The maximum bandwidth for accepted formants.
#' @param fs The sampling frequency of the sound. If a 'sound' or 'ts' object
#' is passed, this does not need to be specified.
#' @param show If TRUE, a plot is created which allows the user to visually
#' inspect the process.
#' @param periodicity A value between 0 and 1. Signal sections with corrected
#' ACF values lower than this are not analyzed. Allows voiceless sections to be
#' excluded from analysis.
#' @param returnbw If TRUE, estimated formant bandwidths are returned.
#' @return A dataframe with the following elements is returned:
#' 
#' \item{time}{The time, in milliseconds, of the middle of the analysis
#' window.} \item{f#}{The formant frequency for formant number #, one column
#' for each formant.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' 
#' data (sound)
#' 
#' par (mfrow = c(2,1), mar = c(4,4,1,1))
#' formanttrack (sound)
#' #formanttrack (sound, periodicity = 0)
#' 
#' 
formanttrack = 
  function (sound, timestep = 5, windowlength = 30, formants = 5, cutoff = 5000, 
            minformant = 200, maxbw = 600, fs = 22050, show = TRUE, 
            periodicity = .5, returnbw = FALSE){

  if (inherits(sound,"ts")) fs = frequency(sound)
  if (inherits(sound,"sound")) {
    fs = sound$fs
    sound = sound$sound
  }     
  if (!is.numeric(sound)) stop ('Sound must be numeric.')
  if (timestep < 0) stop ('Timestep must be positive.')
  if (windowlength < timestep) stop ('Window length must be greater than or equal to timestep.')
  if (cutoff > fs/2) stop ('Cutoff frequency must be less than or equal to the Nyquist frequency.')

  if (cutoff != fs/2){
    sound = resample (sound, cutoff*2, fs)
    fs = cutoff*2
  }
  T = 1 / fs
  stepsize = round ((timestep / 1000) / T)
  half = ceiling (windowlength/1000 * fs)/ 2
  spots = seq (1+half, length(sound)-half, stepsize)

  ffs = NULL
  bws = NULL
  total = 1
  for (i in 1:(length(spots)-1)){
    section = sound[(spots[i]-half):(spots[i]+half)]
    #tmpacf = pitchtrack (section, timestep=0, fs=fs)[2] 
    #if (tmpacf>=periodicity){
      tmp = findformants (section, fs = cutoff*2, maxbw = maxbw, minformant = minformant, 
      verify = F, coeffs = formants*2+3)[1:formants,]
      tmp[is.na(tmp[,1]),] = 0
      ffs = rbind (ffs, c(0,tmp[,1]))
      bws = rbind (bws, tmp[,2])
      ffs[total,1] = spots[i] * (1000/fs)
      total = total + 1
    #}
  }
  colnames(ffs) = c('time',paste ('f', 1:formants, sep=''))
  rownames(ffs) = 1:nrow(ffs)
  
  if (returnbw){
    ffs = cbind (ffs, bws)
    colnames(ffs) = c('time',paste ('f', 1:formants, sep=''),paste ('b', 1:formants, sep=''))
  }
    tmp = data.frame (ffs)
  
  if (show == TRUE){
    spectrogram (sound, colors = FALSE, quality = FALSE, fs = fs)
    for (i in 2:(formants+1)) points (ffs[ffs[,i]!=0,1], ffs[ffs[,i]!=0,i], pch = 16, col = i-1, cex = .75)
  }
  invisible (tmp)
}

