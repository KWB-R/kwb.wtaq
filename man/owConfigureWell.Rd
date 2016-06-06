\name{owConfigureWell}
\alias{owConfigureWell}
\title{Configure well(s) for Optiwells configuration}
\description{Configure well(s) for Optiwells configuration}
\usage{owConfigureWell(wellName, x, y, r, z1, z2, sw)}
\arguments{
  \item{wellName}{well identifier(s). Must be unique within a well field.}
  \item{x}{x coordinate(s) of well centre(s)}
  \item{y}{y coordinate(s) of well centre(s)}
  \item{r}{well radius(es)}
  \item{z1}{Depth(s) below top of aquifer or initial water table to the top of the 
screened interval of the well(s), in units of length.  }
  \item{z2}{Depth(s) below top of aquifer or initial water table to the bottom of the
screened interval of the well(s), in units of length.}
  \item{sw}{Well-bore skin parameter(s), dimensionless. }
}

\value{data frame with one row (per well) and columns elements \emph{wellName},
\emph{x}, \emph{y}, \emph{r}, \emph{z1}, \emph{z2}, \emph{sw} defining a
well.}

\author{Hauke Sonnenberg}





