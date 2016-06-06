\name{wtConfigureAquifer}
\alias{wtConfigureAquifer}
\title{Aquifer parameter values}
\description{Define aquifer-related WTAQ parameters}
\usage{wtConfigureAquifer(aqtype = "WATER TABLE", bb = 20, hkr = 0.0041, 
    hkz = 0.00174, ss = 3.76e-05, sy = 0.25)}
\arguments{
  \item{aqtype}{Type of aquifer being simulated. Two options are provided:
AQTYPE = CONFINED or
AQTYPE = WATER TABLE}
  \item{bb}{Thickness or saturated thickness of aquifer at beginning of simulation, 
in units of length.}
  \item{hkr}{Horizontal hydraulic conductivity of aquifer, in units of length per time.}
  \item{hkz}{Vertical hydraulic conductivity of aquifer, in units of length per time.}
  \item{ss}{Specific storage of aquifer, in units of inverse length.}
  \item{sy}{Specific yield of aquifer, dimensionless. Enter 0 if 
AQTYPE = CONFINED.}
}

\value{list with elements \emph{aqtype}, \emph{bb}, \emph{hkr}, \emph{hkz},
\emph{ss}, \emph{sy}, representing WTAQ parameters related to the aquifer.
See descriptions in Arguments section.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigure}}    }

