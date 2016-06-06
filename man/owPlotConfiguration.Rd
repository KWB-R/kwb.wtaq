\name{owPlotConfiguration}
\alias{owPlotConfiguration}
\title{Plot Optiwells configuration}
\description{Plot Optiwells configuration}
\usage{owPlotConfiguration(owConfiguration, referenceWell = -1, view = c("top", 
    "side"), col = rainbow(nrow(owConfiguration$wellfield)), 
    onePage = TRUE, ...)}
\arguments{
  \item{owConfiguration}{Optiwells configuration as retrieved by \code{\link{owConfigure}}}
  \item{referenceWell}{Number of reference well (according to row number in
owConfiguration$wellfield or -1 (no reference well). If a reference well
is specified, it will be plotted on the left-hand side of the sideview and
the top view will be centered around that well.}
  \item{view}{vector of c("top", "side"). Determines whether to plot top view or side
view or both.}
  \item{col}{colours. Default: rainbow colours}
  \item{onePage}{if TRUE, top view and side view appear below each other on one and the
same page}
  \item{\dots}{
}
}



\author{Hauke Sonnenberg}





