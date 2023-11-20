# Copyright (c) 2015 Santiago Barreda
# All rights reserved.






#' Normalize Vowels
#' 
#' Function to normalize vowels using one of several methods.
#' 
#' This function normalizes vowels based on provided formant frequencies (FFs).
#' The available methods are:
#' 
#' Nearey formant-extrinsic log-mean ('neareyE'): This method finds the
#' logarithmic-mean FF across all vowels produced by a speaker, and subtracts
#' this value from the log-transformed FFs representing each vowel.
#' 
#' Nearey formant-intrinsic log-mean ('neareyI'): This method finds the
#' logarithmic-mean for each formant independently across all vowels produced
#' by a speaker. The log-mean for each individual formant is then subtracted
#' from the log-transformed FF representing each vowel.
#' 
#' Lobanov ('lobanov'): This method finds the mean and standard deviation for
#' each formant. FFs are then standardized (in the statistical sense) using
#' these estimated parameters for each speaker, for each formant.
#' 
#' Watt and Fabricius ('wandf'): This method requires the user to provided
#' point vowels representing the frontmost and highest vowel, and the lowest
#' (and, ideally central) vowel in a vowel system. An estimate of the centroid
#' of the vowel system is calculated based on these values. Normalized FFs are
#' then expressed as the ratio of observed FFs to the estimated centroids,
#' independently for F1 and F2.
#' 
#' For both Nearey methods, and the Lobanov method, the average is found for
#' each vowel category within-speaker before calculating the overall mean. As a
#' result, the data from each speaker may contain unequal numbers of each vowel
#' category. However, all speakers must be represented by the same vowel
#' categories or the result will be (possibly) subtle differences in normalized
#' vowel spaces dues to the possibly differing estimates of means and stadard
#' deviations of the different formants.
#' 
#' @param formants A matrix or dataframe containing formant frequency
#' information for vowels. Each row is assumed to indicate data from a single
#' vowel. At least two columns (indicating information regarding at least two
#' formants) are required.
#' @param speakers A vector indicating which speaker produced each vowel in
#' 'formants'. The length of this vector must equal the number of rows in
#' 'formants'.
#' @param vowels A vector indicating the vowel category of each vowel in
#' 'formants'. The length of this vector must equal the number of rows in
#' 'formants'.
#' @param method A string indicating the desired method. Choices are 'neareyE',
#' 'neareyI', 'lobanov' and 'wandf'. See details for more information.
#' @param corners For the 'wandf' method, a vector of two strings indicating
#' the lowest F1-highest F2 vowel, and the highest F1-intermediate F2 vowel for
#' the vowel system. In most vowel systems these are an /i/-like vowel, and an
#' /a/-like vowel, respectively. Vowels must be provided in that order.
#' @return A dataframe with the same numbers of rows as the formant data
#' provided and the following columns:
#' 
#' \item{formants}{A column corresponding to each formant provided. These are
#' named 'fn' where n corresponds to the formant number.} \item{speakers}{A
#' column indicated which speaker produced each vowel.} \item{vowels}{A column
#' indicating which vowel is represented in each row.}
#' @author Santiago Barreda <sbarreda@@ucdavis.edu>
#' @references Lobanov, B. M. (1971). Classification of Russian vowels spoken
#' by different listeners. Journal of the Acoustical Society of America
#' 49:606-08.
#' 
#' Nearey, T. M. (1978). Phonetic Feature Systems for Vowels. PhD thesis,
#' Indiana University Linguistics Club.
#' 
#' Watt, D. and Fabricius, A. (2002). Evaluation of a technique for improving
#' the mapping of multiple speakers' vowel spaces in the F1 ~ F2 plane. In D.
#' Nelson, Leeds Working Papers in Linguistics and Phonetics 9:159-73.
#' @examples
#' 
#' ## normalize all Peterson & Barney (1952) vowels using each method.
#' data (pb52)
#' 
#' neareyE = normalize (pb52[,7:9], pb52$speaker, pb52$vowel,
#' method = 'neareyE')
#' neareyI = normalize (pb52[,7:9], pb52$speaker, pb52$vowel,
#' method = 'neareyI')
#' lobanov = normalize (pb52[,7:9], pb52$speaker, pb52$vowel,
#' method = 'lobanov')
#' wandf = normalize (pb52[,7:9], pb52$speaker, pb52$vowel,
#' method = 'wandf', corners =  c('i','A'))
#' 
#' ## compare normalization methods using vowelplot().
#' par (mfrow = c(2,2), mar = c(4,4,3,1))
#' vowelplot (neareyE[,1], neareyE[,2], neareyE$vowel, alternateAxes = TRUE, 
#'   pointType = 16, main = 'neareyE', ellipses = TRUE)
#' vowelplot (neareyI[,1], neareyI[,2], neareyI$vowel, alternateAxes = TRUE, 
#'   pointType = 16, main = 'neareyI', ellipses = TRUE)
#' vowelplot (lobanov[,1], lobanov[,2], lobanov$vowel, alternateAxes = TRUE, 
#'   pointType = 16, main = 'lobanov', ellipses = TRUE)
#' vowelplot (wandf[,1], wandf[,2], wandf$vowel, alternateAxes = TRUE, 
#'   pointType = 16, main = 'wandf', ellipses = TRUE)
#' 
normalize = function (formants, speakers, vowels, method = 'neareyE', corners = NULL){
  if (is.null(ncol(formants))) stop("At least two formants must be provided (i.e. F1, F2, ...)")
  if (length(speakers) != nrow (formants)) stop('Speaker vector length does not match formant data length.')
  if (length(vowels) != nrow (formants)) stop('Formant vector length does not match formant data length.')
  if (!(method %in% c('barreda','neareyE', 'neareyI','lobanov','wandf'))) stop ('Invalid method selected. See help file for available methods.')
  
  speakers = as.factor (as.character(speakers))
  vowels = as.factor (as.character(vowels))
  speakersf = levels (speakers)
  vowelsf = levels (vowels)
  if (method == 'wandf') 
  if (sum (corners %in% vowelsf) != 2 | is.null(corners)) stop ('Please provide two corner vowels which are present in the vowel vector.')

  ns = length (speakersf) 
  nv = length (vowelsf)
  nffs = ncol (formants)  
  
  if (method == 'neareyE'){
    if (max(formants) > 30) formants = log (formants)
    meanffs = rowSums(formants)/ncol(formants)
    for (j in 1:ns){
      temp = (speakers == speakersf[j])
      psi = mean (tapply(meanffs[temp], vowels[temp], mean))
      formants[temp, ] = formants[temp, ] - psi
  }}
  if (method == 'neareyI'){
    if (max(formants) > 30) formants = log (formants)
    for (j in 1:ns){
      temp = (speakers == speakersf[j])
      mff = NULL
      for (i in 1:nffs) mff = c(mff, mean (tapply(formants[temp,i], vowels[temp], mean)))
      mffs = matrix (mff, nrow (formants[temp,]), nffs, byrow = TRUE)
      formants[temp, ] = formants[temp, ] - mffs
  }}
  if (method == 'lobanov'){
    for (j in 1:ns){
      temp = (speakers == speakersf[j])
      mff = NULL
      sdff = NULL
      for (i in 1:nffs){ 
         mff = c(mff, mean (tapply(formants[temp,i], vowels[temp], mean)))
         sdff = c(sdff, sd(tapply(formants[temp,i], vowels[temp], mean)))
      }
      mffs = matrix (mff, nrow (formants[temp,]), nffs, byrow = TRUE)
      sdffs = matrix (sdff, nrow (formants[temp,]), nffs, byrow = TRUE)
      formants[temp, ] = (formants[temp, ] - mffs) / sdffs
  }}
  if (method == 'barreda'){
    if (max(formants) > 30) formants = log (formants)
    for (j in 1:ns){
      temp = (speakers == speakersf[j])
      mff = NULL
      sdff = NULL
      for (i in 1:nffs){ 
         mff = c(mff, sum(range (tapply(formants[temp,i], vowels[temp], mean)))/2)
         sdff = c(sdff, mean((mff[i] - tapply(formants[temp,i], vowels[temp], mean))^2)^.5)
      }
      mffs = matrix (mff, nrow (formants[temp,]), nffs, byrow = TRUE)
      sdffs = matrix (sdff, nrow (formants[temp,]), nffs, byrow = TRUE)
      formants[temp, ] = (formants[temp, ] - mffs) / sdffs
  }}  
  if (method == 'wandf'){
    formants = formants[,1:2]
    for (j in 1:ns){
      temp = (speakers == speakersf[j])
      iyf1 = mean (formants[temp & vowels == corners[1], 1])
      iyf2 = mean (formants[temp & vowels == corners[1], 2])
      ahf1 = mean (formants[temp & vowels == corners[2], 1])
      ahf2 = mean (formants[temp & vowels == corners[2], 2])
     
      sf1 = (iyf1 + ahf1 + iyf1) / 3
      sf2 = (iyf2 + ahf2 + iyf1) / 3
     
      formants[temp, 1] = formants[temp, 1] / sf1
      formants[temp, 2] = formants[temp, 2] / sf2
  }}  
  
  output = data.frame (formants, speaker = speakers, vowel = vowels)
  return (output)
}

