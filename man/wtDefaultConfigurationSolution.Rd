\name{wtDefaultConfigurationSolution}
\alias{wtDefaultConfigurationSolution}
\title{default WTAQ solver configuration}
\description{default configuration for the WTAQ solver}
\usage{wtDefaultConfigurationSolution(aqtype, idra = ifelse(aqtype == 
    "CONFINED", 0, 1))}
\arguments{
  \item{aqtype}{Type of aquifer being simulated. Two options are provided:
AQTYPE = CONFINED or
AQTYPE = WATER TABLE}
  \item{idra}{Type of drainage at water table. Enter 0 if AQTYPE = CONFINED. 
Three options are provided:
IDRA = 0: Instantaneous drainage.
IDRA = 1: Gradual drainage.
IDRA = 2: Drainage with unsaturated-zone characterization.}
}



\author{Hauke Sonnenberg}





