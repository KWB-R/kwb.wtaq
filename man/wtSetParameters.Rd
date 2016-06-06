\name{wtSetParameters}
\alias{wtSetParameters}
\title{set parameters in WTAQ configuration}
\description{set numerical, scalar parameters in WTAQ configuration}
\usage{wtSetParameters(configuration, assignments = NULL)}
\arguments{
  \item{configuration}{WTAQ configuration as returned by \code{\link{wtConfigure}}}
  \item{assignments}{list of "name = value" pairs defining the parameter assignments, e.g.
list(hkr = 0.001, hkz = 0.002) to set the horizontal hydraulic 
conductivity (hkr) to 0.0001 (length/time) and the vertical hydraulic 
conductivity (hkz) to 0.00005 (length/time). For the allowed parameter
names see the description in \code{\link{wtSetParameter}}}
}

\value{\emph{configuration} with adapted parameter values as defined in 
\emph{assignments}}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtSetParameter}}}

