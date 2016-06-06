\name{wtReadInputFile}
\alias{wtReadInputFile}
\title{Read WTAQ configuration from input file}
\description{Reads a WTAQ configuration (as e.g. required by
\code{\link{wtRunConfiguration}}) from an existing WTAQ input file.}
\usage{wtReadInputFile(inputFile, dbg = FALSE)}
\arguments{
  \item{inputFile}{full path to an existing WTAQ input file}
  \item{dbg}{if TRUE, debug message are shown, else not.  }
}

\value{list with elements \emph{general}, \emph{aquifer}, \emph{drainage}, 
\emph{times}, \emph{solution}, \emph{pumpwell}, \emph{obswell}, 
representing a WTAQ model run configuration.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtRunInputFile}, \link{wtConfigure}}}

