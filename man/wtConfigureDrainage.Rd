\name{wtConfigureDrainage}
\alias{wtConfigureDrainage}
\title{Drainage parameter values}
\description{Define drainage-related WTAQ parameters}
\usage{wtConfigureDrainage(idra = 2, alpha = c(), acc = 5, akk = 31.7, 
    amm = 100, axmm = 10)}
\arguments{
  \item{idra}{Type of drainage at water table. Enter 0 if AQTYPE = CONFINED. 
Three options are provided:
IDRA = 0: Instantaneous drainage.
IDRA = 1: Gradual drainage.
IDRA = 2: Drainage with unsaturated-zone characterization.}
  \item{alpha}{Only used if idra = 1. Drainage constants, in units of inverse time. 
Maximum of 5 values is allowed.}
  \item{acc}{Only used if idra = 2. Soil-moisture retention exponent, in units of 
inverse length.}
  \item{akk}{Only used if idra = 2. Relative hydraulic-conductivity exponent, in units 
of inverse length. The value specified must be greater than or equal to 
that specified for ACC.}
  \item{amm}{Only used if idra = 2. Initial unsaturated-zone thickness above the 
capillary fringe, in units of length.}
  \item{axmm}{Only used if idra = 2. The unsaturated-zone thickness above the capillary 
fringe above which an assumption of an infinitely thick unsaturated-zone 
thickness is assumed, in units of length.}
}

\value{list with elements \emph{idra}, \emph{alpha}, \emph{acc}, \emph{akk},
\emph{amm}, \emph{axmm}, representing WTAQ parameters related to drainage.
See descriptions in Arguments section.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigure}}}

