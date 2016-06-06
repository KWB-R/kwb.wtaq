\name{wtConfigureObservationWell}
\alias{wtConfigureObservationWell}
\title{Observation well parameter values}
\description{Define WTAQ parameters related to an obervation well}
\usage{wtConfigureObservationWell(obname = "obs1", r = 30, iows = 0, 
    idpr = 0, rp = ifelse(idpr == 0, 0, 0.1), z1 = ifelse(iows == 
        2, 0, 5), z2 = ifelse(iows == 2, 0, 10), xll = ifelse(idpr == 
        0, 0, z2 - z1), zp = ifelse(iows == 2, 3, 0), tsobs = data.frame(t = 60 * 
        1:3, dd = c(0.4, 0.6, 0.7)), irun = 1)}
\arguments{
  \item{obname}{Name of observation well or piezometer; up to 10 characters in length.}
  \item{r}{Radial distance from axis of pumped well to observation well or 
piezometer, in units of length.}
  \item{iows}{Type of observation well or piezometer:
IOWS = 0: Partially penetrating observation well.
IOWS = 1: Fully penetrating observation well.
IOWS = 2: Observation piezometer. Default: 0}
  \item{idpr}{Options for delayed response of observation well.
IDPR = 0: No delayed response. 
IDPR = 1: Delayed response. Default: 0}
  \item{rp}{Inside radius of the observation well (or piezometer) standpipe in the 
interval over which water levels are changing during pumping, in units of 
length. Enter 0 if IDPR = 0. Default: 0.1, but 0 if idpr = 0.}
  \item{z1}{Depth below top of aquifer or initial water table to the top of screened 
interval of observation well, in units of length. Use for IOWS = 0 or 1. 
Enter 0 if IOWS = 2. Default: 5.0, but 0 if iows = 2.}
  \item{z2}{Depth below top of aquifer or initial water table to the bottom of 
screened interval of observation well, in units of length. Use for 
IOWS = 0 or 1. Enter 0 if IOWS = 2. Default: 10.0, but 0 if iows = 2.}
  \item{xll}{Length of screened interval of observation well or piezometer, in units 
of length. Must be 0 if IDPR = 0. Default: z2 - z1, but 0 if idpr = 0. }
  \item{zp}{Depth below top of aquifer or initial water table to center of piezometer, 
in units of length. Use for IOWS = 2. Enter 0.0 if IOWS = 0 or 1.
Default: 3.0, but 0.0 if iows not equal to 2.}
  \item{tsobs}{data.frame with column \emph{t} containing the user-specified times for 
which drawdown at the observation well or piezometer will be calculated.
If the data.frame has no rows, no drawdowns are calculated for the 
observation well or piezometer. The data.frame can (optionally) contain 
the measured drawdown for the observation well or piezometer at the 
corresponding times in an additional column \emph{dd}.   }
  \item{irun}{Option to suppress drawdown calculations for the observation well or 
piezometer. Allows user to specify time-drawdown data, but those data are 
ignored during the simulation. Options are:
IRUN = 0: Drawdowns not calculated.
IRUN = 1: Drawdowns calculated.  }
}

\value{list with elements \emph{irun}, \emph{obname}, \emph{iows}, \emph{idpr},
\emph{r}, \emph{z1}, \emph{z2}, \emph{zp}, \emph{rp}, \emph{xll}, \emph{tsobs},
representing WTAQ parameters related to an observation well. 
See description in Arguments section.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigure}}    }

