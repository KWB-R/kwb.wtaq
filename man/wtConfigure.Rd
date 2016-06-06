\name{wtConfigure}
\alias{wtConfigure}
\title{Define parameter values for WTAQ model run}
\description{Define parameter values for WTAQ model run}
\usage{wtConfigure(general = wtConfigureGeneral(), aquifer = wtConfigureAquifer(), 
    drainage = wtConfigureDrainage(), times = wtConfigureTimes(), 
    solution = wtConfigureSolution(), pumpwell = wtConfigurePumpwell(), 
    obswells = list(wtConfigureObservationWell(obname = "obs1"), 
        wtConfigureObservationWell(obname = "obs2", r = 50)))}
\arguments{
  \item{general}{List of general parameters as retrieved by \code{\link{wtConfigureGeneral}}.}
  \item{aquifer}{List of aquifer parameters as retrieved by \code{\link{wtConfigureAquifer}}.  }
  \item{drainage}{List of drainage parameters as retrieved by \code{\link{wtConfigureDrainage}}.  }
  \item{times}{List of simulation time parameters as retrieved by \code{\link{wtConfigureTimes}}.  }
  \item{solution}{List of solution parameters as retrieved by \code{\link{wtConfigureSolution}}.    }
  \item{pumpwell}{List of pumped well parameters as retrieved by \code{\link{wtConfigurePumpwell}}.    }
  \item{obswells}{List of lists of observation well parameters each of which can be 
retrieved by \code{\link{wtConfigureObservationWell}}.    }
}

\value{list with elements \emph{general}, \emph{aquifer}, \emph{drainage},
\emph{times}, \emph{solution}, \emph{pumpwell}, \emph{obswells},
representing a full WTAQ configuration. For the different elements see the
descriptions in the Arguments section.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigurationExample2}, \link{wtConfigurationExample3}, \link{wtRunConfiguration}}  }

