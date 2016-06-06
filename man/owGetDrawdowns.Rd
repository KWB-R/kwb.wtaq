\name{owGetDrawdowns}
\alias{owGetDrawdowns}
\title{Drawdowns for Optiwells configuration and pump discharges}
\description{Calculate drawdowns for given Optiwells configuration and pump discharges}
\usage{owGetDrawdowns(owConfiguration, Q, times = NULL, solution = NULL, 
    to.matrix = TRUE, ...)}
\arguments{
  \item{owConfiguration}{Optiwells configuration as e.g. retrieved by \code{\link{owRandomConfiguration}}.}
  \item{Q}{vector of discharges at the wells in the order of wells in 
owConfiguration$wellfield}
  \item{times}{vector of times for which drawdowns are to be calculated}
  \item{solution}{List of solution parameters as retrieved by 
\code{\link{wtConfigureSolution}}. If not specified, a default 
configuration, as retrieved by \code{\link{wtDefaultConfigurationSolution}} 
is used.}
  \item{to.matrix}{if TRUE, the results returned by \code{\link{wtRunConfiguration}} that 
come in "long" form (the drawdowns for each well appear in blocks one
below each other with a column "WELL" indicating the well name) are
converted to "matrix" (= wide) form in which the calculated drawdowns
appear in columns one beside the other.}
  \item{\dots}{additional arguments passed to \code{\link{wtRunConfiguration}}, such as
\emph{show.results} or \emph{dbg}, see there.}
}

\value{list with as many elements as there are wells defined in the Optiwells 
configuration (list element \emph{wellfield} of \emph{owConfiguration}). For each
pumping well (well with Q > 0) the list element at the corresponding
position contains a data frame holding the drawdowns calculated by WTAQ
with that well pumping alone and the other wells being observation wells.
For each non-pumping well (well with Q = 0) the list element will be NULL.
This list can then be used to calculate the superposition of drawdowns.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{owSuperposeDrawdowns}}    }

