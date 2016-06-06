\name{wtConfigureSolution}
\alias{wtConfigureSolution}
\title{Solver-related parameter values}
\description{Define WTAQ parameters related to the solver algorithms}
\usage{wtConfigureSolution(isoln = 2, rerrnr = 1e-10, rerrsum = 0, nmax = 0, 
    ntms = 30, ns = 8, error = 1e-04, nnn = 6, method = 3)}
\arguments{
  \item{isoln}{Numerical-inversion solution type:
ISOLN = 1: Solution by the Stehfest algorithm (must use this option for 
confined aquifers).
ISOLN = 2: Solution by the de Hoog algorithm (must use this option for 
drainage with unsaturated-zone characterization [wtConfigureDrainage()$idra == 2]).}
  \item{rerrnr}{Relative error for Newton-Raphson iteration and finite summations of 
drawdown for water-table aquifers. A value of 1.0E-10 is suggested. 
Enter 0.0D0 for AQTYPE = CONFINED.}
  \item{rerrsum}{Only used if isoln = 1. Relative error for finite summations of drawdown 
for confined aquifers. Suggested value is 1.0E-07 to 1.0E-08. Enter 0
if AQTYPE = WATER TABLE.}
  \item{nmax}{Only used if isoln = 1. Maximum number of terms permitted in the finite 
summations of drawdown for confined aquifers. Suggested value is 200. 
Enter 0 if AQTYPE = WATER TABLE.}
  \item{ntms}{Factor used to determine number of terms in the finite summations for 
drawdown for water-table aquifers. Suggested values are 20 or 30. 
Enter 0 if AQTYPE = CONFINED.}
  \item{ns}{Only used if isoln = 1. Number of terms used in the Stehfest algorithm. 
This must be an even integer, the value of which depends upon computer 
precision. If the computer holds 16 significant figures in double 
precision, let NS = 6 to 12. A value of 8 is recommended.}
  \item{error}{Only used if isoln = 2. Relative error sought for the accuracy of the 
numerical inversion. A value of 1.0E-04 is suggested.}
  \item{nnn}{Only used if isoln = 2. Number of terms used in the summation of the 
Fourier series of the approximation to the inverse Laplace transform. 
A value of 6 is suggested.}
  \item{method}{Only used if isoln = 2. Indicates which method will be used to accelerate 
convergence of the Fourier series. Options are 1, 2, or 3. Only METHOD = 3
has been tested and was found to be satisfactory. Users can consult de 
Hoog and others (1982) and John Knight's subroutine LAPADC for additional 
details if needed.}
}

\value{list with elements \emph{isoln}, \emph{rerrnr}, \emph{rerrsum}, \emph{nmax},
\emph{ntms}, \emph{ns}, \emph{error}, \emph{nnn}, \emph{method},
representing WTAQ parameters related to the solving algorithms used in WTAQ.
See descriptions in Arguments section.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigure}}   }

