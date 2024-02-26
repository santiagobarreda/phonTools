

#' Aronson et al. (1996) Hebrew Vowel Data
#' 
#' Formant frequency information for vowels averaged across 6 male speakers.
#'  
#' @name a96
#' @docType data
#' @format A data frame with the following columns: 
#' \itemize{
#'   \item sex A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   \item f3 A numeric vector indicating the vowel F3 in Hz.
#'   \item f4 A numeric vector indicating the vowel F4 in Hz.
#'   }
#' @references Aronson, L., Rosenhouse, J. Rosenhouse, G. & Podoshin, L.
#' (1996). An acoustic analysis of modern Hebrew vowels and voiced consonants.
#' Journal of Phonetics 24. 283-193.
#' @examples
#' 
#' data(a96)
#' vowelplot (a96$f1, a96$f2, a96$vowel, logaxes = 'xy', xsampa = TRUE)
#' 
NULL



#' Bradlow (1995) Spanish Vowel Data
#' 
#' Formant frequency information for vowels averaged across 4 male speakers.
#' 
#' 
#' @name b95
#' @docType data
#' @format A data frame with the following columns: #' 
#' \itemize{
#'   \item sex A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   }
#' @references Bradlow, A. R. (1995). A comparative acoustic study of English
#' and Spanish vowels. Journal of the Acoustical Society of America 97.
#' 1916-1924.
#' @examples
#' 
#' data(b95)
#' vowelplot (b95$f1, b95$f2, b95$vowel, logaxes = 'xy', xsampa = TRUE)
#' 
NULL



#' Fant (1973) Swedish Vowel Data
#' 
#' Formant frequency information for vowels averaged across 50 male speakers.
#' 
#' 
#' @name f73
#' @docType data
#' @format A data frame with the following columns: 
#' \itemize{
#'   \item sex A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   \item f3 A numeric vector indicating the vowel F3 in Hz.
#'   \item f4 A numeric vector indicating the vowel F4 in Hz.
#'   }
#' @references Fant, G. (1973). Speech sounds and features. Cambridge, MA: MIT
#' Press.
#' @examples
#' 
#' data(f73)
#' vowelplot (f73$f1, f73$f2, f73$vowel, logaxes = 'xy', xsampa = TRUE)
#' 
NULL





#' Fourakis et al. (1999) Greek Vowel Data
#' 
#' Formant frequency information for vowels averaged across 5 male speakers.
#' 
#' 
#' @name f99
#' @docType data
#' @format A data frame with the following columns:
#' \itemize{
#'   \item sex A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   }
#' @references Fourakis, M., Botinis, A. & Katsaiti, M. (1999). Acoustic
#' characteristics of Greek vowels. Phonetica, 56. 28-43.
#' @examples
#' 
#' data(f99)
#' vowelplot (f99$f1, f99$f2, f99$vowel, logaxes = 'xy', xsampa = TRUE)
#' 
NULL



#' Hillenbrand et al. (1995) Vowel Data
#' 
#' Formant frequency, f0 and duration information for vowels collected from 139
#' speakers in the Hillenbrand et al. (1995) data. Speaker numbers have been
#' modified to be uniquely identifying numbers. Data has been simplified so
#' that only "steady state" formant frequency measures are given. Missing F2
#' values (n = 10) and F3 values (n = 41) have been imputed using the
#' imputeformants() function included in this package.
#' 
#' 
#' @name h95
#' @docType data
#' @format A data frame with 1668 observations on the following 9 variables:
#' \itemize{
#'   \item sex A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   \item f3 A numeric vector indicating the vowel F3 in Hz.
#'   \item f0 A numeric vector indicating the vowel f0 in Hz.
#'   \item dur A numeric vector indicating the vowel duration in ms.
#'   \item speaker A numeric vector indicating a uniquely identifying speaker number.
#'   \item type A factor with levels b g m and w representing speaker type: boy, girl, man and woman.
#'   }
#' @references Hillenbrand, J.M., Getty, L.A., Clark, M.J., and Wheeler, K.
#' (1995). "Acoustic characteristics of American English vowels," Journal of
#' the Acoustical Society of America, 97, 3099-3111.
#' @source The data was created from data provided on Dr. Hillenbrand's
#' personal website:
#' 
#' http://homepages.wmich.edu/~hillenbr/voweldata.html
#' @examples
#' 
#' data(h95)
#' vowelplot (h95$f1, h95$f2, h95$vowel, logaxes = 'xy', ellipses = TRUE,
#' xsampa = TRUE)
#' 
NULL




#' Pols et al. (1973) Dutch Vowel Data
#' 
#' Formant frequency information for vowels averaged across 24 male speakers.
#' 
#' 
#' @name p73
#' @docType data
#' @format A data frame with the following 9 variables:
#' \itemize{
#'   \item sex A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   \item f3 A numeric vector indicating the vowel F3 in Hz.
#' }
#' @references Pols, L. C. W., Tromp, H. R. C., & Plomp, R. (1973). Frequency
#' analysis of Dutch vowels from 50 male speakers. Journal of the Acoustical
#' Society of America, 53. 1093-1101.
#' @examples
#' 
#' data(p73)
#' vowelplot (p73$f1, p73$f2, p73$vowel, logaxes = 'xy', ellipses = TRUE, 
#' xsampa = TRUE)
#' 
NULL





#' Peterson & Barney (1952) Vowel Data
#' 
#' Formant frequency and f0 information for vowels collected from 76 speakers
#' in the Peterson & Barney (1952) data.
#' 
#' 
#' @name pb52
#' @docType data
#' @format A data frame with 1520 observations on the following 9 variables:
#' \itemize{
#'   \item sex A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   \item f3 A numeric vector indicating the vowel F3 in Hz.
#'   \item f0 A numeric vector indicating the vowel f0 in Hz.
#'   \item dur A numeric vector indicating the vowel duration in ms.
#'   \item repetition A numeric vector indicating the repetition number.
#'   \item speaker A numeric vector indicating a uniquely identifying speaker number.
#'   \item type A factor with levels c m and w representing speaker type: child, man and woman.
#'   }
#' @references Peterson, G.E. & Barney (1952). Control methods used in a study
#' of the vowels. Journal of the Acoustical Society of America 24: 175-184.
#' 
#' Boersma, Paul & Weenink, David (2012). Praat: doing phonetics by computer
#' [Computer program]. Version 5.3.19, retrieved 24 June 2012 from
#' http://www.praat.org/
#' @source The data was created from tables provided within Praat:
#' 
#' http://www.fon.hum.uva.nl/praat/
#' @examples
#' 
#' data(pb52)
#' vowelplot (pb52$f1, pb52$f2, pb52$vowel, logaxes = 'xy', ellipses = TRUE, 
#' xsampa = TRUE)
#' 
NULL





#' Sound object
#' 
#' An example of a sound object, the phrase 'This is a spectrogram', produced
#' by the author of this package. This sound object may be inspected with
#' spectrogram() and plotted with plot(). Sections of the individual samples
#' representing the waveform (found in the "sound" element of a sound object)
#' can be passed to spectralslice() to see a spectral slice. Several functions
#' included in this package may also be used to modify or manipulate 'sound'
#' objects.
#' 
#' 
#' @name sound
#' @docType data
#' @format This sound object has the same properties as all sound objects.
#' These may be inspected by using the print() function.
#' @examples
#' 
#' 
#' ## uncomment and run
#' 
#' #data (sound)
#' #par (mar = c(4,4,1,1))
#' #multiplot (n = 3, sizes = c(.25, .5, .25))  
#' 
#' #plot (sound)
#' #spectrogram (sound, dynamicrange = 50, maxfreq = 7000)
#' 
NULL





#' Thomson (2007) Vowel Data
#' 
#' Mean formant frequency information for the vowels of male and female
#' speakers from Edmonton English.
#' 
#' 
#' @name t07
#' @docType data
#' @format A data frame with 1520 observations on the following 9 variables:
#' \itemize{
#'   \item gender A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   \item f3 A numeric vector indicating the vowel F3 in Hz.
#'   \item f4 A numeric vector indicating the vowel F4 in Hz.
#'   }
#' @references Thomson, R. (2007). Modeling L1/L2 interactions in the
#' perception and production of English vowels by Mandarin L1 speakers: A
#' training study. PhD dissertation, University of Alberta.
#' @examples
#' 
#' data(t07)
#' vowelplot (t07$f1, t07$f2, t07$vowel, logaxes = 'xy', meansOnly = TRUE, 
#' xsampa = TRUE)
#' 
NULL





#' Information about Vowel Data Sets
#' 
#' Contains a brief description and listing of all vowel data sets included in
#' this package.
#' 
#' 
#' @name voweldata
#' @docType data
#' @format A data frame with the following information: 
#' \itemize{
#'   \item name The name of the data set.
#'   \item language The language represented in the data set.
#'   \item nspeakers The number of speakers
#'   \item sexes The sexes of the speakers, m (male) and female (f).
#'   \item nformants The number of formants represented, between 2 and 4.
#'   \item individual If 'yes', individual speaker measurements are given, if 'no', measurements are averaged within gender.
#' } 
#' @references At the moment 9 vowel data sets are included in this package.
#' See individual data set pages for citation information.
#' @examples
#' 
#' data(voweldata)
#' 
NULL





#' Yang (1996) Korean Vowel Data
#' 
#' Formant frequency information for vowels averaged across 60 male and female
#' speakers.
#' 
#' 
#' @name y96
#' @docType data
#' @format A data frame with the following columns: 
#' \itemize{
#'   \item sex A factor indicating speaker sex.
#'   \item vowel The vowel category in x-sampa.
#'   \item f1 A numeric vector indicating the vowel F1 in Hz.
#'   \item f2 A numeric vector indicating the vowel F2 in Hz.
#'   \item f3 A numeric vector indicating the vowel F3 in Hz.
#'   }
#' @references Yang, B. (1996). A comparative study of American English and
#' Korean vowels produced by male and female speakers. Journal of Phonetics,
#' 24. 245-261.
#' @examples
#' 
#' data(y96)
#' vowelplot (y96$f1, y96$f2, y96$vowel, logaxes = 'xy', xsampa = TRUE)
#' 
NULL



