\name{wtPlotConfiguration}
\alias{wtPlotConfiguration}
\title{plot wellfield profile}
\description{plot wellfield profile}
\usage{wtPlotConfiguration(configuration = wtConfigurationExample3(), 
    col.pw = "black", col.ow = rainbow(length(configuration$obswells)), 
    main = "WTAQ Configuration with Pumping Well on the Left", 
    asp = 1, ...)}
\arguments{
  \item{configuration}{configuration as retrieved by \code{\link{wtConfigure}}, 
default: wtConfigurationExample3()}
  \item{col.pw}{pumping well colour, default: "black"}
  \item{col.ow}{observation well colours. Default: rainbow colours}
  \item{main}{title for the plot}
  \item{asp}{aspect ratio, see ?plot  }
  \item{\dots}{further arguments passed to .plotSideView}
}



\author{Hauke Sonnenberg}





