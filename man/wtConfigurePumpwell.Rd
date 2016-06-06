\name{wtConfigurePumpwell}
\alias{wtConfigurePumpwell}
\title{Pumped well parameter values}
\description{Define WTAQ parameters related to the pumped well}
\usage{wtConfigurePumpwell(ipws = 0, ipwd = 1, rw = 1, rc = ifelse(ipwd == 
    0, 0, rw), zpd = 5, zpl = 10, sw = 0, qq = 0.05, tspw = data.frame(t = 60 * 
    1:3, dd = 2 * c(0.4, 0.6, 0.7)), pwname = "PW", ipump = 1, 
    irun = 1)}
\arguments{
  \item{ipws}{Type of pumped well:
IPWS = 0: Partially penetrating pumped well.
IPWS = 1: Fully penetrating pumped well. Default: 0}
  \item{ipwd}{Type of diameter of pumped well:
IPWD = 0: Infinitesimal diameter (line-source theory).
IPWD = 1: Finite diameter. Default: 1}
  \item{rw}{Radius of pumped well screen, in units of length. Default: 1.0}
  \item{rc}{Inside radius of pumped well in the interval where water levels are 
changing during pumping, in units of length. Must be 0 if IPWD = 0.
Default: value of rw, but 0 if ipwd = 0.}
  \item{zpd}{Depth below top of aquifer or initial water table to the top of the 
screened interval of the pumped well, in units of length. Default: 5.0}
  \item{zpl}{Depth below top of aquifer or initial water table to the bottom of the 
screened interval of the pumped well, in units of length. Default: 10.0}
  \item{sw}{Well-bore skin parameter, dimensionless. Default: 0.0}
  \item{qq}{Pumping rate of well, in units of cubic length per time. Default: 0.05}
  \item{tspw}{data.frame with column \emph{t} containing the user-specified times for 
which drawdown at the pumped well will be calculated. If the data.frame
has no rows, no drawdowns are calculated for the pumped well. The 
data.frame can (optionally) contain the drawdowns that have been measured
for the pumped well at the corresponding times in an additional column 
\emph{dd}. }
  \item{pwname}{Name of pumping well. This parameter is not evaluated by WTAQ but will be 
used in the result data frame returned by
\code{\link{wtRunConfiguration}}. We introduce it in order to be able to
identify the well within a wellfield. Default: "PW"}
  \item{ipump}{Option to suppress calculations of drawdown at pumped well:
IPUMP = 0: Drawdown is not calculated at pumped well.
IPUMP = 1: Drawdown is calculated at pumped well.  }
  \item{irun}{Option to suppress drawdown calculations for the pumped well. Allows user 
to specify time-drawdown data, but those data are ignored during the 
simulation. Options are:
IRUN = 0: Drawdowns not calculated.
IRUN = 1: Drawdowns calculated.  }
}

\value{list with elements \emph{irun}, \emph{ipws}, \emph{ipwd}, \emph{ipump}, 
\emph{qq}, \emph{rw}, \emph{rc}, \emph{zpd}, \emph{zpl}, \emph{sw},
\emph{tspw}, representing WTAQ parameters related to the pumped well. See
descriptions in Arguments section.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigure}}}

