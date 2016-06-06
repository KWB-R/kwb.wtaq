\name{owRandomConfiguration}
\alias{owRandomConfiguration}
\title{Random Optiwells well field configuration}
\description{Returns a random Optiwells configuration on which e.g.
\code{\link{owGetDrawdowns}} can be run.}
\usage{owRandomConfiguration(numberOfWells, bb = 80, ...)}
\arguments{
  \item{numberOfWells}{number of wells in the well field (must be a number between 1 and 25).}
  \item{bb}{Thickness or saturated thickness of aquifer at beginning of simulation, 
in units of length.  }
  \item{\dots}{additional arguments that are passed to owRandomWellfield, such as: 
\emph{rmean}, \emph{z1mean}, \emph{z2mean}, \emph{swmean}, \emph{rsd},
\emph{z1sd}, \emph{z2sd}, \emph{swsd}, \emph{digits}, see
\code{\link{owRandomWellfield}}}
}

\value{list with elements \emph{wellfield} (configuration of well
characteristics), \emph{aquifer} (WTAQ aquifer configuration) and
\emph{drainage} (WTAQ drainage configuration), just as created by
\code{\link{owConfigure}}}

\author{Hauke Sonnenberg}



\seealso{\code{\link{owRandomWellfield}, \link{owConfigure}}}

