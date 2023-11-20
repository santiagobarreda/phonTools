\name{pickIPA}
\alias{pickIPA}
\alias{ipainfo}
\title{
Pick IPA Symbols
}
\description{
Select IPA symbols that you wish to include in a plot.
}
\usage{
pickIPA (vowels, n = 0, xsampa = FALSE, description = FALSE, 
         verify = TRUE)
}
\arguments{
  \item{vowels}{An optional vector of vowel labels that you would like to plot using IPA symbols.} 
  \item{n}{If no vowel vector is provided, the number of symbols desired.} 
  \item{xsampa}{If TRUE, x-sampa versions of the symbols are returned.} 
  \item{description}{If TRUE, description of the place, manner and voicing of the symbols are also returned.} 
  \item{verify}{If TRUE, the selected symbols are plotted in the order indicated by the user, allowing visual confirmation of the selected symbols.} 
}
\details{
This is an interactive function that allows the user to select IPA symbols for plotting using a chart. The values returned by this function may only be passed to the 'pch' parameter within plotting functions. At the moment it has only been implemented for vowel sounds.

If a vowels vector is given, the function finds the number of categories in the vector. The user is then prompted to select the IPA symbol corresponding to each category by clicking on the correct location on the plot.  

If a vowels vector is not provided, the function allows the user to select any desired number of symbols, and returns these in the same order as indicated by the user. 

**There may be issues when exporting figures to PDF using IPA font. Exporting plots directly as images works 'out of the box'**
}
\value{
A list with the following columns (some of which are optional):

\item{IPA}{The symbol which should be passed to 'pch' to plot IPA symbols.}
\item{xsampa}{The xsampa representation of each IPA character.}
\item{description}{If description = TRUE, a description of each sound.}
}
\references{
http://en.wikipedia.org/wiki/X-SAMPA
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
##uncomment to run
#vowels = pickIPA (n = 3)
#plot (c(1,2,3), c(1,2,3), pch = vowels)

## select vowels in the order displayed in the console
## to obtain symbols and descriptions of the vowel categories 
## in the Peterson and Barney data. 

# data (pb52)
# tmp = pickIPA (pb52$vowel, description = TRUE, xsampa = TRUE)
# tmp
}
