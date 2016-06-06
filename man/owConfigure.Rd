\name{owConfigure}
\alias{owConfigure}
\title{Definition of an Optiwells well field configuration }
\description{Returns a user-defined Optiwells configuration on which e.g.
\code{\link{owGetDrawdowns}} can be run.}
\usage{owConfigure(wellfield = NULL, aquifer = NULL, drainage = NULL)}
\arguments{
  \item{wellfield}{Optiwells wellfield configuration. Data frame }
  \item{aquifer}{WTAQ aquifer configuration as returned by \code{\link{wtConfigureAquifer}}}
  \item{drainage}{WTAQ drainage configuration as returned by \code{\link{wtConfigureDrainage}}}
}

\value{list with elements \emph{wellfield} (configuration of well
characteristics), \emph{aquifer} (WTAQ aquifer configuration) and
\emph{drainage} (WTAQ drainage configuration).}

\author{Hauke Sonnenberg}



\seealso{\code{\link{owRandomConfiguration}}}

