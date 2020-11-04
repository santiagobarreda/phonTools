\name{voweldata}
\alias{voweldata}
\docType{data}
\title{
Information about Vowel Data Sets
}
\description{
Contains a brief description and listing of all vowel data sets included in this package. 
}
\usage{data (voweldata)
}
\format{
  A data frame with the following information:
  \describe{
    \item{\code{name}}{ - The name of the data set.}
    \item{\code{language}}{ - The language represented in the data set.}
    \item{\code{nspeakers}}{ - The number of speakers.}
    \item{\code{sexes}}{ - The sexes of the speakers, m (male) and female (f).}
    \item{\code{nformants}}{ - The number of formants represented, between 2 and 4.}
    \item{\code{individual}}{ - If 'yes', individual speaker measurements are given, if 'no', measurements are averaged within gender.}
  }
}
\references{
At the moment 9 vowel data sets are included in this package. See individual data set pages for citation information.
}
\examples{
data(voweldata)
}
