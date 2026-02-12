
#' Vowel Synthesis
#' 
#' Create synthetic vowels using a cascade formant synthesizer.
#' 
#' This function is a Klatt-style cascade formant synthesizer that is intended
#' to create synthetic vowel sounds. The voice source is generated using the
#' KLGLOTT88 method described in Klatt (1988).
#' 
#' If the vowels sound too 'robotic', there may be formants too close to the
#' Nyquist frequency. Conversely, if the vowels sound too 'muffled', you may
#' need to add more formants, and make sure they extend closer to the Nyquist
#' frequency. Vowels sound more natural when f0 is not static.
#' 
#' Formant bandwidths may be provided in Hz, or as a percentage of the formant
#' frequencies. To set these as a percent of formant frequencies, all values
#' must be less than 1. If these are not provided they are set to 6 percent of
#' the formant center frequencies by default. If only one value is provided,
#' this is assumed to be the desired value for all formants.
#' 
#' @export
#' @param ffs A vector of center frequencies for each formant. For moving
#' formants, a list containing two vectors may be provided, where the first
#' vector indicates intitial values, and the final vector indicates final
#' values. Formant frequencies should be provided in order of lowest to highest
#' frequency.
#' @param fbw A vector of formant bandwidths for each formant. See details for
#' more information.
#' @param dur The desired duration of the sound, in milliseconds. Vowels must
#' be at least 50 ms long.
#' @param f0 The desired f0 (pitch) of the sound. Optionally, a vector with
#' initial and final f0 values may be provided.
#' @param f0precision An integer specifying the precision which which f0 differences can be expressed. 
#' @param fs The desired sampling frequency of the output sound.
#' @param verify If TRUE, the waveform and spectrogram of the created sound are
#' plotted to allow the user to visually verify the process.
#' @param returnsound If TRUE, the sound is returned as a sound object, which
#' can be used with several other functions included in this package. If FALSE,
#' only a vector representing the sound wave is returned.
#' @param noisesd Standard deviation of noise to be added to the source, as
#' a proportion of the source RMS amplitude.
#' @param noiseshape If TRUE, noise amplitude is shaped inversely with glottal
#' opening to simulate natural voice source characteristics.
#' @param power A desired power contour can be specified as a numeric vector.
#' Must be of the same length as the output sound, or the sound is truncated
#' to the length of this vector. If NULL, a default envelope is generated.
#' @param preemph If TRUE, the output sound is preemphasized with a coefficient
#' of 0.94.
#' @return A vector or 'sound' object representing the filtered sound.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Klatt, D. H. (1980). Software for a cascade/parallel formant
#' synthesizer. Journal of the Acoustical Society of America 67(3): 971-995.
#' 
#' Klatt, D. H. (1988). Klattalk: The conversion of English text to speech.
#' Unpublished Manuscript. Massachusetts Institute of Technology, Cambridge,
#' MA. Chapter 3.
#' @examples
#' \dontrun{
#' 
#' ## The following examples are based on my vowels 
#' i = vowelsynth (returnsound = FALSE, f0 = c(125,105))
#' a = vowelsynth (ffs = c(700, 1300, 2300, 3400, 4400), 
#' returnsound = FALSE, f0 = c(125,105))
#' e = vowelsynth (ffs = c(400, 2000, 2600, 3400, 4400), 
#' returnsound = FALSE, f0 = c(125,105))
#' o = vowelsynth (ffs = c(400, 900, 2300, 3400, 4400), 
#' returnsound = FALSE, f0 = c(125,105))
#' u = vowelsynth (ffs = c(300, 750, 2300, 3400, 4400), 
#' returnsound = FALSE, f0 = c(125,105))
#' 
#' silence = rep(0, 1000)
#' vowels = c(a, silence, e, silence, i, silence, o, silence, u)
#' 
#' writesound (vowels, filename = 'vowels.wav', fs = 10000)
#' 
#' # an example of a synthetic diphthong
#' ei = vowelsynth (ffs = list(c(400, 2000, 2600, 3400, 4400), 
#' c(270, 2200, 2800, 3400, 4400)), f0 = c(125,105))
#' writesound (ei)
#' spectrogram (ei, pause = FALSE)
#' }
#' 

vowelsynth = function (ffs = c(270, 2200, 2800, 3400, 4400), 
                       fbw = 0.06, dur = 300, f0precision = 10,
                       f0 = c(120, 100), fs = 10000, verify = FALSE, 
                       returnsound = TRUE, noisesd = 0.5, 
                       power = NULL, noiseshape = TRUE,preemph=TRUE){

  T = 1/fs
  n = round(dur/1000/T)
  if (!is.null(power)) 
    n = length(power)
  if (is.numeric(nrow(ffs))) 
    n = nrow(ffs)
  if (length(f0) == 1) 
    f0 = c(f0, f0)
  f0 = exp(seq(log(f0[1]), log(f0[2]), length.out = n))
  # Pre-interpolate f0 to high precision for smooth contour
  f0_interp = approx(1:n, f0, xout = seq(1, n, length.out = n * f0precision))$y
  
  vsource = NULL
  while (length(vsource) < n * f0precision) { ## 5
    # Index into f0_interp based on current position in output
    current_idx = min(length(vsource) + 1, length(f0_interp))
    f0_current = f0_interp[current_idx]
    cycle = fs / f0_current
    tmp = 2 * seq(0, 1, 1/(round(cycle*(f0precision-1)))) - 3 * seq(0, 1, 1/(round(cycle *(f0precision-1))))^2  # 4 ->9
    tmp = c(rep(0, cycle), tmp)
    vsource = c(vsource, tmp)
  }
  vsource = phonTools::resample(vsource, fs, fs * f0precision)

  cycle = fs/mean (f0)/4
  noiseamp = filter(vsource, rep(1 / cycle, cycle), sides = 2)
  noiseamp = noiseamp - min(noiseamp, na.rm=TRUE)
  noiseamp = noiseamp / max(noiseamp, na.rm=TRUE)
  noiseamp[is.na (noiseamp)] = 0
  
  #par (mfrow = c(1,2), mar = c(4,4,1,1))
  #plot (vsource, type="l")
  #lines (noise, col=2)
  #plot (vsource, type="l")
  
  vsource = vsource[1:n]
  vsource = jitter(vsource)
  
  if (preemph) vsource = phonTools::preemphasis(vsource, coeff = 0.94)
  
  x = c(1, 20/(1000/fs), 40/(1000/fs), 50/(1000/fs), n - (30/(1000/fs)), n)
  x = pmin(pmax(x, 1), n)  # Clamp envelope points to valid range [1, n]
  if (is.null(power)) 
    power = phonTools::interpolate(x, y = c(10, 30, 55, 60, 55, 30), increment = 1, 
                        type = "linear")[1:n, 2]
  power = 10^(power/20)
  power = jitter(power, factor = 0.01)
  vsource = vsource * power
  
  # Filter voiced source through formants
  output = phonTools::Ffilter(vsource, ffs = ffs, fs = fs, verify = FALSE, 
                   bwp = fbw)
  output = output * power
  
  # Add aspiration component: filter noise through same formants
  aspiration = rnorm(length(vsource))
  if (noiseshape) aspiration = aspiration * (1 - noiseamp[1:length(vsource)])
  aspiration = aspiration / sd(aspiration) * sd(vsource)
  aspiration = aspiration * power * noisesd * 0.5  # Scale aspiration relative to voicing
  aspiration = phonTools::Ffilter(aspiration, ffs = ffs, fs = fs, verify = FALSE, 
                         bwp = fbw)
  #aspiration = phonTools::preemphasis(aspiration, coeff = -0.94)

  
  # Mix aspiration with voiced output
  output = output + aspiration
  
  output = output/(max(abs(output)) * 1.05)
  if (returnsound == TRUE) 
    output = phonTools::makesound(output, "sound.wav", fs = fs)
  return(output)
}
