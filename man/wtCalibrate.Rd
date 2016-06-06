\name{wtCalibrate}
\alias{wtCalibrate}
\title{Simple calibration procedure}
\description{Simple calibration procedure}
\usage{wtCalibrate(configuration, parameterName = "hkr", startvalue = 0.02, 
    endvalue = 1e-05, changevalue = 1e-04, wells = "OW4", fitnessCriteria = data.frame(name = c("RMSE", 
        "PBIAS", "PBIAS", "R2"), vals = c(0.03, 5, -5, 0.95), 
        condition = c("<=", "<=", ">=", ">=")))}
\arguments{
  \item{configuration}{
}
  \item{parameterName}{
}
  \item{startvalue}{
}
  \item{endvalue}{
}
  \item{changevalue}{
}
  \item{wells}{
}
  \item{fitnessCriteria}{stat. parameters for for model fit evaluation}
}



\author{Hauke Sonnenberg}





