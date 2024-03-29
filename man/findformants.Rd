% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findformants.R
\name{findformants}
\alias{findformants}
\title{Find Formants}
\usage{
findformants(
  sound,
  fs = 10000,
  coeffs = NULL,
  maxbw = 600,
  minformant = 200,
  verify = TRUE,
  showbws = FALSE,
  showrejected = TRUE
)
}
\arguments{
\item{sound}{A numeric vector representing a waveform, or a 'sound' object
created with the loadsound() or makesound() functions.}

\item{fs}{The sampling frequency of the sound. If a 'sound' object is passed
this does not need to be specified.}

\item{coeffs}{If a number is given, this many coefficients are used in the
analysis. Alternatively, the LPC (AR filter) coefficients may be passed to
the function directly using this parameter. For good results, two
coefficients are required for each formants plus 2 or 3 'for the pot'.}

\item{maxbw}{The maximum bandwidth for accepted formants.}

\item{minformant}{Formants below this frequency are rejected.}

\item{verify}{If TRUE, a plot is created which allows the user to visually
inspect the process.}

\item{showbws}{If TRUE, formant bandwidths are indicated on the plot.}

\item{showrejected}{If TRUE, rejected formant locations are indicated.}
}
\value{
A dataframe with the following elements is returned:

\item{frequency}{The frequencies of formants, in ascending order.}
\item{bandwidth}{The corresponding formant bandwidth.}
}
\description{
Find formants given a sound or set of LPC coefficients.
}
\details{
Formant frequencies are found analytically using the formulas provided in
Snell (1993). If Verify = TRUE, the estimated frequency response, formant
locations, and a pole-zero plot of the estimated filter are presented.
Accepted formants are presented in 5 colors (which are reused if there are
more than 5 formants), while rejected formants are presented in black.
}
\examples{
\dontruin{
# make a synthetic vowel with a known set of 
# formant frequencies and bandwidths
sound = vowelsynth (ffs = c(500,1500,2500,3500,4500),
                    fbw = c(30, 90, 150, 210, 270), f0 = 100)

# compare different plotting options					
findformants (sound)
findformants (sound, showrejected = FALSE)
findformants (sound, showbws = TRUE)
}
}
\references{
Snell, R.(1993). "Formant location from LPC analysis data", IEEE
Transactions on Speech and Audio Processing, 1(2), pp. 129-134.
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
