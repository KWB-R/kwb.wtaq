\name{wtCheckConfiguration}
\alias{wtCheckConfiguration}
\title{check WTAQ configuration}
\description{Checks a WTAQ configuration for its structure}
\usage{wtCheckConfiguration(configuration, part = "complete", dbg = TRUE)}
\arguments{
  \item{configuration}{WTAQ configuration (complete) or sub-configuration (general, aquifer, 
rainage, times, solution, pumpwell, obswell)}
  \item{part}{one of c("complete", "general", "aquifer", "drainage", "times",
"solution", "pumpwell", "obswell")}
  \item{dbg}{if TRUE, debug messages are shown, else not.}
}

\value{Error message or "" if no error occurred}

\author{Hauke Sonnenberg}




\examples{

# Generate default configuration
cconf <- wtConfigure()  

# Check (complete) configuration. No error -> ok
wtCheckConfiguration(cconf) 

# Generate default observation well configuration
oconf <- wtConfigureObservationWell()

# Check "obswell" configuration. No error -> ok
wtCheckConfiguration(oconf, "obswell") 

# Set obswells in complete configuration (forgetting that obswells must be
# a list!):
cconf$obswells <- oconf 

# Check (complete) configuration again 
# -> Error when "Checking config of observation well 1".
wtCheckConfiguration(cconf) 
}
