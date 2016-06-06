\name{wtConfigureTimes}
\alias{wtConfigureTimes}
\title{Simulation time parameter values}
\description{Define WTAQ parameters related to the times to be simulated}
\usage{wtConfigureTimes(its = 1, tlast = 0, nlc = 0, nox = 0)}
\arguments{
  \item{its}{Time specification:
ITS = 0: Log-cycle time steps (use to generate theoretical curves).
ITS = 1: User-specified times.}
  \item{tlast}{Largest value of time. Enter 0 if ITS = 1.}
  \item{nlc}{Number of logarithmic cycles on the time scale for which drawdown will 
be calculated. Enter 0 if ITS = 1.}
  \item{nox}{Number of equally spaced times per logarithmic cycle for which drawdown 
will be calculated. Enter 0 if ITS = 1.}
}

\value{list with elements \emph{its}, \emph{tlast}, \emph{nlc}, \emph{nox},
representing WTAQ parameters defining the times to be simulated.
See descriptions in Arguments section.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigure}}   }

