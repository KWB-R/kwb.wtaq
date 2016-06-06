\name{wtConfigurationExample3}
\alias{wtConfigurationExample3}
\title{WTAQ configuration representing WTAQ example 3}
\description{WTAQ configuration corresponding to sample problem 3 of WTAQ distribution}
\usage{wtConfigurationExample3()}

\value{list with elements \emph{general}, \emph{aquifer}, \emph{drainage},
\emph{times}, \emph{solution}, \emph{pumpwell}, \emph{obswells},
representing a full WTAQ configuration. }

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigurationExample2}, \link{wtConfigure}}}
\examples{
# Get configuration of sample problem 3 of WTAQ distribution
cnf <- wtConfigurationExample3()

# Print formatted output of configuration
print(cnf)

# Plot wellfield profile of configuration
wtPlotConfiguration(cnf)
}
