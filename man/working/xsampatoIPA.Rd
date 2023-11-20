\name{xsampatoIPA}
\alias{xsampatoIPA}
\title{
Convert X-Sampa to IPA
}
\description{
Convert a vector of x-sampa vowel descriptions to values that can be used to plot IPA.
}
\usage{
xsampatoIPA (vowels, chart = FALSE, verify = FALSE)
}
\arguments{
  \item{vowels}{A vector representing vowel labels in x-sampa to be converted to values that can be used to plot the corresponding IPA symbols. If this is not provided a plot comparing IPA to x-sampa representations is displayed.} 
  \item{chart}{If TRUE, a plot comparing IPA to x-sampa representations is displayed.} 
  \item{verify}{If TRUE, the selected symbols are plotted to allow for verification.} 
}
\details{
This function converts x-sampa to values that can be used to plot IPA symbols by passing them to the 'pch' parameter within plotting functions. At the moment it has only been implemented for vowel sounds. The function also generate a figure comparing IPA to x-sampa vowel representations that may be useful to some users.

**There may be issues when exporting figures to PDF using IPA font. Exporting plots directly as images works 'out of the box'**
}
\value{
A vector of the same length as 'vowels', with the values required to plot the desired vowels. 
}
\references{
http://en.wikipedia.org/wiki/X-SAMPA
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{

## compare x-sampa and IPA vpwel charts
#xsampatoIPA ()
## some examples
#IPA = xsampatoIPA (c('I','3','e','Q'))
#plot (1:5, pch = IPA, cex = 2)

}
