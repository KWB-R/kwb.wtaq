\name{wtInputFileLines}
\alias{wtInputFileLines}
\title{text lines for WTAQ input file}
\description{This function transforms a WTAQ configuration as generated with
\code{\link{wtConfigure}} into a vecotor of text lines. These text lines,
written to a file, can be used as input file to the WTAQ drawdown modelling
software.}
\usage{wtInputFileLines(configuration = wtConfigure(), sep = "\\t\\t", 
    dbg = FALSE)}
\arguments{
  \item{configuration}{WTAQ configuration as generated by \code{\link{wtConfigure}}.}
  \item{sep}{Separator to be placed between parameter values and parameter names.
Default: two tab characters.  }
  \item{dbg}{if TRUE, debug message are shown, else not. Default: FALSE}
}

\value{character vector with each element representing one row of the input file.}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtReadInputFile}}}

