\name{owWellDistances}
\alias{owWellDistances}
\title{Distances between wells}
\description{Calculates distances between wells based on coordinates given in Optiwells
configuration of the wellfield}
\usage{owWellDistances(wellfield, referenceWell = 1)}
\arguments{
  \item{wellfield}{data frame as e.g. retrieved by \code{\link{owRandomWellfield}} with each
row representing a well of the well field.}
  \item{referenceWell}{Number of reference well corresponding with row number in 
\emph{wellfield}. For each well the distance to the reference well is
calculated. Default: 1}
}

\value{vector of numeric representing the distances of each well to a reference
well (determined by \emph{referenceWell})}

\author{Hauke Sonnenberg}





