\name{wtConfigurationExample2}
\alias{wtConfigurationExample2}
\title{WTAQ configuration representing WTAQ example 2}
\description{WTAQ configuration corresponding to sample problem 2 of WTAQ distribution}
\usage{wtConfigurationExample2()}

\value{list with elements \emph{general}, \emph{aquifer}, \emph{drainage},
\emph{times}, \emph{solution}, \emph{pumpwell}, \emph{obswells},
representing a full WTAQ configuration. }

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtConfigurationExample3}, \link{wtConfigure}}}
\examples{
# Get configuration of sample problem 2 of WTAQ distribution
cnf <- wtConfigurationExample2()

# Print formatted output of configuration
print(cnf)

# Plot wellfield profile of configuration
wtPlotConfiguration(cnf)
}
