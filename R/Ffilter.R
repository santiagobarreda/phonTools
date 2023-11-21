# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Formant Filtering
#' 
#' Perform cascade formant filtering of sounds.
#' 
#' This function allows the user to specify one or more formant filters and to
#' pass a signal through said filters. This may be used to create synthetic
#' speech sounds or to modify existing sounds as desired. The given signal is
#' passed through the formant filters in reverse order. Filter bandwidths
#' specify the distance between formant center frequencies and the point at
#' which the output will be 3 dB below peak energy.
#' 
#' Filter bandwidths may be provided in Hz, or as a percentage of the formant
#' frequencies. To set these as a percent of formant frequencies, all values
#' must be less than 1. If these are not provided they are set to 6 percent of
#' the formant center frequencies by default. If only one value is provided,
#' this is assumed to be the desired value for all formants.
#' 
#' @param sound A numeric vector representing a waveform, or a 'sound' object
#' created with the loadsound() or makesound() functions.
#' @param ffs A list of vectors of initial and final center frequencies for
#' each formant. Each vector should contain formant frequencies in order of
#' lowest to highest frequency. If only a single vector is provided, formants
#' will remain stable throughout.
#' @param bwp A vector of formant bandwidths, one for each formant.
#' @param minbw The minimum permissible formant bandwidth, in Hertz.
#' @param fs The sampling frequency of the sound. If a 'sound' object is
#' passed, this does not need to be specified.
#' @param verify If TRUE, before and after spectra are plotted to allow the
#' user to visually verify the process.
#' @return A vector representing the filtered sound.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Klatt, D. H. (1980). Software for a cascade/parallel formant
#' synthesizer. Journal of the Acoustical Society of America 67(3): 971-995.
#' 
#' http://www.fon.hum.uva.nl/praat/manual/Sound__Filter__one_formant____.html
#' @examples
#' 
#' 
#' ## uncomment and run
#' ## Generate half a second of white noise
#' #sound = rnorm (11025, 0, 1)
#' 
#' ## pass this through some formant filters
#' ## two static formants
#' #filtered1 = Ffilter (sound, ffs = c(4000,7000), bw = 500)
#' ## a single moving formant
#' #filtered2 = Ffilter (sound, ffs = list(3000,8000), bw = 500)
#' ## two moving formants
#' 
#' 
#' ## inspect the results
#' #par (mfrow = c(1,2), mar = c(4,4,1,1))
#' #spectrogram (filtered1, maxfreq = 11025)
#' #spectrogram (filtered2, maxfreq = 11025)
#' 
#' 
Ffilter = function (sound, ffs, bwp = 0.06, minbw = 60, fs = 22050, verify = FALSE){
  if (missing(ffs)) 
    stop("At least one formant center frequency must be provided.")
  
  ffsspecified = FALSE
  if (is.numeric(nrow(ffs))) ffsspecified = TRUE  
  bwsspecified = FALSE
  if (is.numeric(nrow(bwp))) bwsspecified = TRUE
  
  soundout = 0; tsout = 0;  
  if (inherits(sound,"ts")){
    fs = frequency(sound)
    tsout = 1
  }
  if (inherits(sound,"sound")) {
    fs = sound$fs
    oldsound = sound
    sound = sound$sound
    soundout = 1
  }  
  if (!ffsspecified){
    if (is.numeric(ffs)) ffs = list(ffs, ffs)
    nffs = length(ffs[[1]])
    if (length(ffs) != 2) 
      stop("Only initial and final formant values may be provided.")
    if (length(ffs[[1]]) != length(ffs[[2]])) 
      stop("Same number of formants must be provided for beginning and\n end points.")
    tmp = matrix(0, length(sound), nffs)
    for (i in 1:nffs) tmp[, i] = seq(ffs[[1]][i], ffs[[2]][i], length.out = length(sound))
    ffs = tmp
  }
  if (ffsspecified) nffs = ncol (ffs)
    
  if (length(bwp) == 1) bwp = rep(bwp, nffs)
  
  percent = TRUE
  if (max(bwp) > 1) percent = FALSE
  output = sound * 0
  T = 1/fs
  old = sound
  new = old * 0
  for (j in nffs:1) {
    CF = ffs[, j]
    if (percent & !bwsspecified){ 
      BW = (CF * bwp[j])
      BW[BW<minbw] = minbw
    }    
    if (!percent & !bwsspecified) BW = rep(bwp[j], length(sound))
    if (bwsspecified) BW = bwp[,j]
    
    C = -exp(-2 * pi * BW * T)
    B = 2 * exp(-pi * BW * T) * cos(2 * pi * CF * T)
    A = 1 - B - C
    new[1] = old[1] * A[1]
    new[2] = old[2] * A[2] - B[2] * new[1]
    for (i in 3:length(old)) new[i] = old[i] * A[i] + 
      new[i-1] * B[i] * sqrt(A[i]/A[i - 1]) + 
      new[i - 2] * C[i] * sqrt(A[i]/A[i - 1])
    old = new
  }
  if (verify == TRUE) {
    oldpar = par()
    par(mfrow = c(2, 1))
    spectralslice(sound, fs = fs, ylim = c(-75, 5))
    spectralslice(new, fs = fs, ylim = c(-75, 5))
    suppressWarnings (par (oldpar))
  }
  if (soundout == 1){
    oldsound$sound = new 
    invisible (oldsound)
  }
  else if (tsout == 1){
    out = ts (new, frequency = fs, start = 0)
    invisible (out)
  }
  else invisible (new)
}
