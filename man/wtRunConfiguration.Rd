\name{wtRunConfiguration}
\alias{wtRunConfiguration}
\title{Run WTAQ with given configuration}
\description{Run a WTAQ simulation with the given configuration}
\usage{wtRunConfiguration(configuration, wtaq.exe = .wtaq_path(), targetDirectory = tempdir(), 
    show.results = FALSE, fileExtension = "", dbg = FALSE, ...)}
\arguments{
  \item{configuration}{WTAQ configuration, as retrieved by \code{\link{wtConfigure}}.}
  \item{wtaq.exe}{full path to WTAQ executable (default: compiled executable in package
subfolder "extdata" as defined in helper function .wtaq_path() )}
  \item{targetDirectory}{optional. Target directory. If no target directory is given, a temporary
directory will be used.}
  \item{show.results}{if TRUE, the content of the results file will be shown in the R console.
Default: FALSE}
  \item{fileExtension}{extension given to files}
  \item{dbg}{if TRUE, debug message are shown, else not. Default: FALSE}
  \item{\dots}{further arguments passed to wtRunInputFile, e.g.
\emph{show.output.on.console}}
}

\value{model result as read with \code{\link{wtReadPlotFile}} from the output
file generated by the WTAQ modelling software}

\author{Hauke Sonnenberg}



\seealso{\code{\link{wtRunInputFile}}}

