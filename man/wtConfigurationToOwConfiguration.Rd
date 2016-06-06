\name{wtConfigurationToOwConfiguration}
\alias{wtConfigurationToOwConfiguration}
\title{wtConfigurationToOwConfiguration}
\description{Generate Optiwells configuration from WTAQ configuration}
\usage{wtConfigurationToOwConfiguration(wtaqConfiguration)}
\arguments{
  \item{wtaqConfiguration}{WTAQ configuration as retrieved by \code{\link{wtConfigure}}}
}

\value{list with elements \emph{wellfield} (configuration of well
characteristics), \emph{aquifer} (WTAQ aquifer configuration) and
\emph{drainage} (WTAQ drainage configuration), just as creaetd by
\code{\link{owConfigure}}. The list elements \emph{aquifer} and 
\emph{drainage} are copied from the given WTAQ configuration. The 
wellfield configuration is generated from the information on the pumping
well and on the observation wells given in that same configuration.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{owConfigurationToWtConfiguration}}}

