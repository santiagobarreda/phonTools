\name{loadtable}
\alias{loadtable}
\title{
Load Table
}
\description{
Load text table data quickly.
}
\usage{
loadtable (...)
}
\arguments{
  \item{...}{Arguments are passed to the internal call of read.table().} 
}
\details{
This function is a wrapper for read.table() for those times when you just don't feel like typing a filename. The function opens up a file selection dialog box allowing the user to select the file containing the data. 
}
\value{
Returns the output of read.table().
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
## uncomment and run
# data = loadtable ()
# head (data)
}
