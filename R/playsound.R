# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Play Sounds
#' 
#' Play sounds in R using VLC Player.
#' 
#' The sound is written out as a .wav file and VLC is called from the command
#' line to play the file. The file is optionally erased after the fact. If the
#' path is selected by the user, this is returned for use in future calls.
#' Obviously, this function relies on VLC being installed. With the appropriate
#' path, it seems like this function should work on Linux and OSX, though it
#' has only been tested on a Windows computer.
#' 
#' @aliases playsound play
#' @param sound Either a sound object, or a numeric vector to be played.
#' @param path The location of VLC.exe on your computer. 'default' provides the
#' default for a standard windows installation of VLC player. If this is set to
#' 'pick', a dialog box opens allowing the user to specify the path.
#' @param fs The sampling frequency in Hz. If a 'sound' or 'ts' object is
#' passed this does not need to be specified.
#' @param erase If TRUE, the temporary WAV file created is deleted after
#' playing.
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @examples
#' 
#' #  ## make a 1 second, 100 Hz tone.
#' #  tone = sinusoid (freqs = 250, dur = 1000, fs = 1000)[,2]
#' #  ## play it in VLC player
#' #  playsound (q[,2], fs = 1000, erase = FALSE)
#' 
playsound = function (sound, path = 'default', fs = 10000, erase = TRUE){
  if (inherits(sound,'sound') & !inherits(sound,'ts')){
    sound = sound / (max(sound)*1.05)
    sound = makesound (sound, 'play_tmp.wav', fs = fs)
  }
  writesound (sound, 'play_tmp.wav')

  pathout = FALSE
  if (path == 'default') path = "C:\\Program Files (x86)\\VideoLAN\\VLC\\vlc.exe"
  if (path == 'pick'){
    path = file.choose()
    pathout = TRUE
  }
  
  if (!file.exists (path)) stop ('VLC player is not installed or incorrect path.')
  system(paste(shQuote(path), '--play-and-exit','play_tmp.wav'),ignore.stderr=TRUE)
  if (erase) unlink ('play_tmp.wav')
  if (pathout) return (path)
}

  
play = function (sound, path = 'default', fs = 10000, erase = TRUE){
  cl = match.call()
  args = sapply (2:length(cl), function(x) cl[[x]])
  names(args) = names(cl)[-1]
  do.call (playsound, args)
}

