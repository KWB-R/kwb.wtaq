\name{owConfigurationToWtConfiguration}
\alias{owConfigurationToWtConfiguration}
\title{WTAQ configuration from Optiwells configuration}
\description{convert Optiwells configuration to WTAQ configuration with one well
being selected as pumping well}
\usage{owConfigurationToWtConfiguration(owConfiguration, activeWell, 
    Q, times = NULL, solution = wtConfigureSolution())}
\arguments{
  \item{owConfiguration}{Optiwells configuration as created with \code{\link{owConfigure}}}
  \item{activeWell}{number of active well = row number in \emph{owConfiguration$wellfield}}
  \item{Q}{discharge of active well}
  \item{times}{vector of times for which drawdowns are to be calculated    }
  \item{solution}{List of solution parameters as retrieved by
\code{\link{wtConfigureSolution}}.}
}

\value{WTAQ configuration as e.g. returned by \code{\link{wtConfigure}}}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigurationToOwConfiguration}}  }

