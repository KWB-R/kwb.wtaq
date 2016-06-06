\name{wtConfiguredDistances}
\alias{wtConfiguredDistances}
\title{distances of observation wells}
\description{Vector of distances between pumping well and observation wells (including
0 as first distance!)}
\usage{wtConfiguredDistances(configuration)}
\arguments{
  \item{configuration}{WTAQ configuration as retrieved by \code{\link{wtConfigure}}.}
}

\value{named vector of numeric containing zero as first element and the distances
between pumping well and observation wells as following elements, in the
order or their definition in the WTAQ configuration. The names of the
elements correspond to the well names as retrieved also with 
\code{\link{wtConfiguredWellnames}}}

\author{Hauke Sonnenberg}





