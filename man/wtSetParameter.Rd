\name{wtSetParameter}
\alias{wtSetParameter}
\title{set parameter in WTAQ configuration}
\description{set numerical, scalar parameter in WTAQ configuration}
\usage{wtSetParameter(configuration, parameterName, parameterValue)}
\arguments{
  \item{configuration}{WTAQ configuration as returned by \code{\link{wtConfigure}}  }
  \item{parameterName}{parameter name. Must be one of the Aquifer parameters "bb", "hkr", "hkz", 
"ss", "sy" (see \code{\link{wtConfigureAquifer}} or one of the Drainage 
parameters "acc", "akk", "amm", "axmm" (see
\code{\link{wtConfigureDrainage}}) or one of the Pumpwell parameters
"qq", "rw", "rc", "zpd", "zpl", "sw" (see \code{\link{wtConfigurePumpwell}})}
  \item{parameterValue}{numeric value to which the parameter \emph{parameterName} shall be set}
}

\value{\emph{configuration} with the parameter \emph{parameterName} altered to
the value given in \emph{parameterValue}}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtSetParameters}}}

