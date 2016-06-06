\name{wtPlotResult}
\alias{wtPlotResult}
\title{Plot WTAQ results}
\description{Plot WTAQ results}
\usage{wtPlotResult(wtaqResult, main = "", plottype = "w", showMeasurements = TRUE, 
    auto.key = NULL, asp = NA, PDF = FALSE, PNG = FALSE, pumpingWellName = "PW", 
    xlim = NULL, ylim = NULL, ...)}
\arguments{
  \item{wtaqResult}{data frame as returned by \code{\link{wtRunConfiguration}}}
  \item{main}{plot title, default: ""}
  \item{plottype}{vector of plot types ("s" = superposed, "w" = one plot per well,
"d" = along distance to pump well, "t" each time along distance to well).
Default: "w" (one plot per well).}
  \item{showMeasurements}{if TRUE, measurements are shown}
  \item{auto.key}{given to xyplot, see there. If NULL, a default key with as many columns as
there are wells is used. Default: NULL.}
  \item{asp}{aspect ratio between x and y axis. Default: 1. Set to NA if aspect ratio
does not matter.  }
  \item{PDF}{if TRUE, a pdf file is created in tempdir() and opened in a PDF viewer}
  \item{PNG}{if TRUE, all plots made with plot type "t" are saved to png files in
tempdir()/wtaqResult.}
  \item{pumpingWellName}{name of pumping well in wtaqResult}
  \item{xlim}{
}
  \item{ylim}{
}
  \item{\dots}{additional arguments given to xyplot}
}



\author{Hauke Sonnenberg}





