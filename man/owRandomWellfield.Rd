\name{owRandomWellfield}
\alias{owRandomWellfield}
\title{Generation of a random wellfield}
\description{Generation of a random wellfield}
\usage{owRandomWellfield(numberOfWells, rmean, z1mean, z2mean, swmean, 
    rsd = 0.1 * rmean, z1sd = 0.1 * z1mean, z2sd = 0.1 * z2mean, 
    swsd = 0.1 * swmean, digits = 1)}
\arguments{
  \item{numberOfWells}{number of wells in the well field (must be a number between 1 and 25).}
  \item{rmean}{mean value of normal distribution used to generate random values for 'r'
(well radius)}
  \item{z1mean}{mean value of normal distribution used to generate random values for 'z1'
(depth of begin of screen)}
  \item{z2mean}{mean value of normal distribution used to generate random values for 'z2'
(depth of end of screen)}
  \item{swmean}{mean value of normal distribution used to generate random values for 'sw'
(well-bore skin parameter)}
  \item{rsd}{standard deviation of normal distribution used to generate random values
for 'r' (well radius). Default: 0.1 * rmean}
  \item{z1sd}{standard deviation of normal distribution used to generate random values
for 'z1'-values' (depth of begin of screen). Default: 0.1 * z1mean}
  \item{z2sd}{standard deviation of normal distribution used to generate random values
for 'z2'-values' (depth of end of screen). Default: 0.1 * z2mean}
  \item{swsd}{standard deviation of normal distribution used to generate random values
for 'sw' (well-bore skin parameter). Default: 0.1 * swmean}
  \item{digits}{number of decimal digits for values of r, z1, z2. Default: 1}
}

\value{data frame with each row representing a well of the well field and the 
columns representing the well properties: \emph{wellName} (well name),
\emph{x} (x-coordinate of well), \emph{y-coordinate of well}, \emph{r}
(well radius), \emph{z1} (depth of begin of well screen), \emph{z2} (depth
of end of well screen), \emph{sw} (well-bore skin parameter)}

\author{Hauke Sonnenberg}





