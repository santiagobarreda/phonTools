
#' Pitch Tracking
#' 
#' Create a pitch track for a sound.
#' 
#' Pitch tracking is carried out using a simplified version of the algorithm
#' described in Boersma (1993), including corrections for lag value and window
#' function. When plotting pitch tracks, the points sizes are proportional to
#' autocorrelation values.
#' 
#' @export
#' @param sound A numeric vector representing a waveform, or a 'sound' object
#' created with the loadsound() or makesound() functions.
#' @param f0range A numeric vector of length two where the first value
#' corresponds to the minimum f0 Hz to be considered, and the second represents
#' the maximum to be considered.
#' @param timestep How far the analysis window will advance, in milliseconds.
#' If this value is set to zero, the function is applied to the entire signal.
#' @param fs The sampling frequency of the sound. If a 'sound' or 'ts' object
#' is passed, this does not need to be specified.
#' @param minacf Autocorrelation values below this are ignored.
#' @param correction If TRUE, ACF values are corrected for lag value.
#' @param show If TRUE, a plot displaying the pitch track is created.
#' @param windowlength The length of the analysis window, in milliseconds. This
#' should be approximately three-times longer than the wavelength of the lowest
#' pitch. The default value is appropriate for a floor of 60 Hz.
#' @param addtospect If TRUE, the pitch track is added to a spectrogram created
#' with the spectrogram() function included in this package. The track is
#' scaled up by a factor of 10 (e.g., 100 Hz will be plotted at 1000 Hz on the
#' spectrogram) so that it will fit nicely in the typical spectrogram range of
#' 0-5000 Hz.
#' @return A dataframe with the following columns:
#' 
#' \item{time}{ The analysis window centre point, in milliseconds.} \item{f0}{
#' The calculated f0 (pitch).} \item{acf}{ The value of the autocorrelation
#' function corresponding to the winning f0.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Boersma, P., (1993). Accurate short-term analysis of the
#' fundamental frequency and the harmonics-to-noise ratio of a sampled sound.
#' Proc. Instit. Phon. Sci. 17: 97-110.
#' @examples
#' 
#' 
#' data (sound)               ## use the example 'sound' object provided
#' #sound = loadsound()       ## or run this line to use you own sound
#' 
#' ## to generate a pitch track
#' output = pitchtrack (sound)
#' 
#' ## to add a pitch to a spectrogram
#' #spectrogram (sound)
#' #pitchtrack (sound, addtospect = TRUE)
#'  
#' 
pitchtrack = function (sound, f0range = c(60,400), timestep = 2, fs = 22050, minacf = .5,
                       correction = TRUE, show = TRUE, windowlength = 50, addtospect = FALSE){
  if (length(f0range) != 2) stop ('A highest and lowest f0 must be specified.')
  if (f0range[2] < f0range[1]) stop ('Maximum f0 must be greater than minimum f0.')
  if (minacf < 0) stop ('minacf must be positive.')
  if (timestep<0) stop ('Timestep must be positive.')
 
  if (inherits(sound,"ts")) fs = frequency(sound)
  if (inherits(sound,"sound")) {
    fs = sound$fs
    sound = sound$sound
  }  
  if (!is.numeric(sound)) stop ('Sound must be numeric.')

  T = 1 / fs
  stepsize = round ((timestep / 1000) / T)
  minlag = round (1 / (T * f0range[2]))
  maxlag = round (1 / (T * f0range[1] ))

  if (timestep>0){
    half = round( ceiling (windowlength/1000 * fs)/ 2)
    spots = seq (1+half, length(sound)-half, stepsize)
    corr = rep (0, length(spots))
    lag = rep (0, length(spots))
    for (i in 1:length(spots)){
      section = sound[(spots[i]-half):(spots[i]+half)] 
      acf = fastacf (section, lag.max = maxlag, show = F, correct = correction)    
      peaks = peakfind (acf[,2], show = FALSE)
      lag[i] = peaks[order(acf$acf[peaks], decreasing = TRUE)[1]]
      if (is.na(lag[i])) lag[i] = 0
      if (lag[i]!=0) corr[i] = acf$acf[lag[i]]
      if (lag[i]==0) corr[i] = 0
    }
    lag[lag < minlag | lag > maxlag] = 0
    spots = spots * T * 1000
    f0 = 1 / (lag * T)
    f0 [f0 == Inf] = 0
    f0 [f0 == fs] = 0

    use = (f0 != 0 & corr > minacf)
    spots = spots[use]; corr = corr[use]; f0 = f0[use];

    if (show & !addtospect) plot (spots, f0, cex = 1.2*corr, pch = 16, col = 4, ylab = 'f0 (Hz)', 
    xlab = 'Time (ms)', ylim = c(0,f0range[2]))
  
    if (addtospect) points (spots, f0*10, col = 6, pch = 16, cex = 1.2*corr)
    output =  data.frame (time = round(spots,1), f0 = round(f0,2), acf = round(corr,4))
  }
  if (timestep==0){
    acf = fastacf (sound, lag.max = maxlag, show = F, correct = correction)    
    peaks = peakfind (acf[,2], show = FALSE)
    lag = peaks[order(acf$acf[peaks], decreasing = TRUE)[1]]
    if (is.na(lag)) lag = 0
    if (lag < minlag) lag = 0
    f0 = 1 / (lag * T)
    f0 [f0 == Inf] = 0
    acf = acf$acf[lag]
    if (lag==0) acf = 0
    output = (c(f0, acf))
	names (output) = c('f0','acf')
  } 
  invisible (output) 
}
  
  
