\name{combocalc}
\alias{combocalc}
\title{
Combinations and Permutations
}
\description{
Calculate the number of combinations or permutations for a number of objects.
}
\usage{
combocalc (objects, choose, order = FALSE, repetition = TRUE)
}
\arguments{
  \item{objects}{The number of different kinds of objects available for selection.} 
  \item{choose}{The number of objects selected at a given time.} 
  \item{order}{If TRUE, the order of the objects matters, for example aba != aab.} 
  \item{repetition}{If TRUE, a sequence such as bbb is permissible.} 
}
\author{
Santiago Barreda <sbarreda@ucdavis.edu>
}
\examples{
# combinations, no repetition
combocalc (10, 4, order = FALSE, repetition = FALSE)
# combinations, with repetition
combocalc (10, 4, order = FALSE, repetition = TRUE)
# permutations, no repetition
combocalc (10, 4, order = TRUE, repetition = FALSE)
# permutations, with repetition
combocalc (10, 4, order = TRUE, repetition = TRUE)
}
