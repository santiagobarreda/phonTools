# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Load WAV files into R
#' 
#' A function which allows WAV files to be loaded into R.
#' 
#' The function is only compatible with 8 and 16 bit mono WAV files. The
#' function returns a 'sound object'. Many of the functions included in this
#' package interact with 'sound' objects.
#' 
#' @aliases loadsound print.sound plot.sound
#' @param filename A string indicating the file name of the WAV file to be
#' loaded. If no filename is provided, a dialog box will open allowing the user
#' to select a file.
#' @return An object of class 'sound', a list containing the elements:
#' 
#' \item{filename}{A vector containing the filename of the WAV file.}
#' \item{fs}{The sampling frequency of the sound. } \item{duration}{The
#' duration of the sound, in milliseconds. } \item{sound}{A vector of numeric
#' values representing the sampled sound. }
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references https://ccrma.stanford.edu/courses/422/projects/WaveFormat/
#' @examples
#' 
#' ## Use the command below to select a WAV file to load into R
#' ## sound = loadsound ()
#' 
#' ## sound
#' ## plot (sound)
#' ## spectrogram (sound)
#' 
loadsound = function (filename=0){
  if (mode(filename)!="character") filename = file.choose()

  soundfile = file(filename,"rb")
  readChar(soundfile, nchars = 8)        ## ChunkId and ChunkSize (4,4)
  if(readChar(soundfile, nchars = 4) != 'WAVE'){
    close(soundfile)
    stop ("File provided is not in .wav format.")
  }

  readBin(soundfile, "integer", n = 10, size = 1)  ## Subchunk1ID, Subchunk1Size, AudioFormat (4,4,2)
  
  numChannels = readBin(soundfile, "integer", n = 1, size = 2)
  if (numChannels > 1){
   stop ("This function only loads mono (single-channel) WAV files.")
  }
  sampleRate = readBin(soundfile,"integer", n = 1, size = 4)

  readBin(soundfile,"integer",n= 6,size=1)   ## ByteRate, BlockAlign (4,2)         

  bitsPerSample = readBin(soundfile, "integer", size = 2)
  if (bitsPerSample > 16) stop ("This function only loads 8 and 16 bit WAV files.")

  readBin(soundfile,"integer",n= 4,size=1)  ## Subchunk2ID
  subchunk2Size = readBin(soundfile,"integer", size=4)

  if (bitsPerSample == 8)
      sound = (readBin (soundfile, "integer" , n = subchunk2Size, size = 1, signed = FALSE) - 128) / 255
  if (bitsPerSample == 16)
      sound = readBin(soundfile,"integer", n = subchunk2Size/2, size=2, signed = TRUE) / 32768
 
  close(soundfile)

  numSamples = subchunk2Size / (bitsPerSample/8)
  fs = sampleRate

  output = list (filename = filename, fs = fs, numSamples = numSamples,
  duration = numSamples/fs * 1000, sound = ts(sound, frequency = fs, start=0))
  class(output) = "sound"
  output  
}
