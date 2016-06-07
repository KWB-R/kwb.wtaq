# owConfigureWell --------------------------------------------------------------
owConfigureWell <- function # Configure well(s) for Optiwells configuration
### Configure well(s) for Optiwells configuration
(
  wellName,
  ### well identifier(s). Must be unique within a well field.
  x,
  ### x coordinate(s) of well centre(s)
  y,
  ### y coordinate(s) of well centre(s)
  r,
  ### well radius(es)
  z1,
  ### Depth(s) below top of aquifer or initial water table to the top of the 
  ### screened interval of the well(s), in units of length.  
  z2, 
  ### Depth(s) below top of aquifer or initial water table to the bottom of the
  ### screened interval of the well(s), in units of length.
  sw
  ### Well-bore skin parameter(s), dimensionless. 
)
{
  data.frame(wellName = wellName, x = x, y = y, r = r, z1 = z1, z2 = z2, sw = sw,
             stringsAsFactors = FALSE)
  
  ### data frame with one row (per well) and columns elements \emph{wellName},
  ### \emph{x}, \emph{y}, \emph{r}, \emph{z1}, \emph{z2}, \emph{sw} defining a
  ### well.
}

# owRandomConfiguration --------------------------------------------------------
owRandomConfiguration <- function # Random Optiwells well field configuration
### Returns a random Optiwells configuration on which e.g.
### \code{\link{owGetDrawdowns}} can be run.
(
  numberOfWells, 
  ### number of wells in the well field (must be a number between 1 and 25).
  bb = 80,
  ### Thickness or saturated thickness of aquifer at beginning of simulation, 
  ### in units of length.  
  ...
  ### additional arguments that are passed to owRandomWellfield, such as: 
  ### \emph{rmean}, \emph{z1mean}, \emph{z2mean}, \emph{swmean}, \emph{rsd},
  ### \emph{z1sd}, \emph{z2sd}, \emph{swsd}, \emph{digits}, see
  ### \code{\link{owRandomWellfield}}
) 
{
  ##seealso<< \code{\link{owRandomWellfield}, \link{owConfigure}}
  
  .stopOnTooBigWellNumber(numberOfWells)
  
  n <- numberOfWells
  
  owConfigure(
    wellfield = owRandomWellfield(numberOfWells = n, ...), 
    aquifer = wtConfigureAquifer(bb = bb), 
    drainage = wtConfigureDrainage())

  ### list with elements \emph{wellfield} (configuration of well
  ### characteristics), \emph{aquifer} (WTAQ aquifer configuration) and
  ### \emph{drainage} (WTAQ drainage configuration), just as created by
  ### \code{\link{owConfigure}}
}

# .stopOnTooBigWellNumber ------------------------------------------------------
.stopOnTooBigWellNumber <- function(numberOfWells)
{
  if (numberOfWells < 1 || numberOfWells > 25) {
    stop("The number of wells must be between 1 and 25.")
  }
}

# owRandomWellfield ------------------------------------------------------------
owRandomWellfield <- function # Generation of a random wellfield
### Generation of a random wellfield
(
  numberOfWells, 
  ### number of wells in the well field (must be a number between 1 and 25).
  rmean, 
  ### mean value of normal distribution used to generate random values for 'r'
  ### (well radius)
  z1mean, 
  ### mean value of normal distribution used to generate random values for 'z1'
  ### (depth of begin of screen)
  z2mean, 
  ### mean value of normal distribution used to generate random values for 'z2'
  ### (depth of end of screen)
  swmean, 
  ### mean value of normal distribution used to generate random values for 'sw'
  ### (well-bore skin parameter)
  rsd = 0.1 * rmean, 
  ### standard deviation of normal distribution used to generate random values
  ### for 'r' (well radius). Default: 0.1 * rmean
  z1sd = 0.1 * z1mean, 
  ### standard deviation of normal distribution used to generate random values
  ### for 'z1'-values' (depth of begin of screen). Default: 0.1 * z1mean
  z2sd = 0.1 * z2mean, 
  ### standard deviation of normal distribution used to generate random values
  ### for 'z2'-values' (depth of end of screen). Default: 0.1 * z2mean
  swsd = 0.1 * swmean, 
  ### standard deviation of normal distribution used to generate random values
  ### for 'sw' (well-bore skin parameter). Default: 0.1 * swmean
  digits = 1
  ### number of decimal digits for values of r, z1, z2. Default: 1
)
{
  .stopOnTooBigWellNumber(numberOfWells)
  
  wellName <- sprintf("W%02d", 1:numberOfWells)
  x <- sample(300, numberOfWells)
  y <- sample(300, numberOfWells)
  r <-  round(rnorm(numberOfWells, rmean, rsd), digits)
  z1 <- round(rnorm(numberOfWells, z1mean, z1sd), digits)
  sw <- round(rnorm(numberOfWells, swmean, swsd), 5)
  
  # guarantee that z2 is greater than z1
  z2 <- round(z1 + abs(rnorm(numberOfWells, z2mean, z2sd) - z1), digits)
  
  # guarantee that sw is positive
  sw <- pmax(0, sw)
  
  owConfigureWell(
    wellName = wellName, x = x, y = y, r = r, z1 = z1, z2 = z2, sw = sw)  
  
  ### data frame with each row representing a well of the well field and the 
  ### columns representing the well properties: \emph{wellName} (well name),
  ### \emph{x} (x-coordinate of well), \emph{y-coordinate of well}, \emph{r}
  ### (well radius), \emph{z1} (depth of begin of well screen), \emph{z2} (depth
  ### of end of well screen), \emph{sw} (well-bore skin parameter)
}

# owConfigure --------------------------------------------------------------
owConfigure <- function # Definition of an Optiwells well field configuration 
### Returns a user-defined Optiwells configuration on which e.g.
### \code{\link{owGetDrawdowns}} can be run.
(
  wellfield = NULL,
  ### Optiwells wellfield configuration. Data frame 
  aquifer = NULL,
  ### WTAQ aquifer configuration as returned by \code{\link{wtConfigureAquifer}}
  drainage = NULL
  ### WTAQ drainage configuration as returned by \code{\link{wtConfigureDrainage}}
) 
{
  ##seealso<< \code{\link{owRandomConfiguration}}
  
  if (is.null(wellfield)) {
    stop(paste("Parameter 'wellfield' is undefined. Create a wellfield by",
               "binding well configurations as created with owConfigureWell", 
               "to a data frame using rbind. See example in tutorial."))
  }
  
  numberOfWells <- nrow(wellfield)
  .stopOnTooBigWellNumber(numberOfWells)
  
  if (is.null(aquifer))
  {
    stop(paste("Parameter 'aquifer' is undefined. Create a WTAQ aquifer",
         "configuration by using wtConfigureAquifer."))
  }
  
  if (is.null(drainage)) {
    stop(paste("Parameter 'drainage' is undefined. Create a WTAQ drainage",
               "configuration by using wtConfigureDrainage."))
  }
  
  list(wellfield = wellfield,
       aquifer = aquifer,
       drainage = drainage)
  
  ### list with elements \emph{wellfield} (configuration of well
  ### characteristics), \emph{aquifer} (WTAQ aquifer configuration) and
  ### \emph{drainage} (WTAQ drainage configuration).
}

# wtConfigurationToOwConfiguration ---------------------------------------------
wtConfigurationToOwConfiguration <- function
### Generate Optiwells configuration from WTAQ configuration
(
  wtaqConfiguration
  ### WTAQ configuration as retrieved by \code{\link{wtConfigure}}
)
{
  ##seealso<< \code{\link{owConfigurationToWtConfiguration}}
    
  distances <- wtConfiguredDistances(wtaqConfiguration)
  
  n <- length(distances)
  y <- 0
  
  wellfield <- .pumpingWellToWellfieldWell(
    wtaqConfiguration$pumpwell, 
    x = distances[1], # = 0
    y = y)  
  
  for (i in seq(from = 1, by = 1, length.out = n - 1)) {
    
    well.new <- .observationWellToWellfieldWell(
      wtaqConfiguration$obswells[[i]], 
      x = distances[i + 1], 
      y = y, 
      sw = wtaqConfiguration$pumpwell$sw)
    
    wellfield <- rbind(wellfield, well.new)
  }

  owConfigure(
    wellfield = wellfield,
    aquifer = wtaqConfiguration$aquifer,
    drainage = wtaqConfiguration$drainage)
  
  ### list with elements \emph{wellfield} (configuration of well
  ### characteristics), \emph{aquifer} (WTAQ aquifer configuration) and
  ### \emph{drainage} (WTAQ drainage configuration), just as creaetd by
  ### \code{\link{owConfigure}}. The list elements \emph{aquifer} and 
  ### \emph{drainage} are copied from the given WTAQ configuration. The 
  ### wellfield configuration is generated from the information on the pumping
  ### well and on the observation wells given in that same configuration.
}

# owWellDistances ---------------------------------------------------------------
owWellDistances <- function # Distances between wells
### Calculates distances between wells based on coordinates given in Optiwells
### configuration of the wellfield
(
  wellfield, 
  ### data frame as e.g. retrieved by \code{\link{owRandomWellfield}} with each
  ### row representing a well of the well field.
  referenceWell = 1
  ### Number of reference well corresponding with row number in 
  ### \emph{wellfield}. For each well the distance to the reference well is
  ### calculated. Default: 1
)
{
  dx <- wellfield$x - wellfield$x[referenceWell]
  dy <- wellfield$y - wellfield$y[referenceWell]
  sqrt(dx*dx + dy*dy)
  
  ### vector of numeric representing the distances of each well to a reference
  ### well (determined by \emph{referenceWell})
}

# .checkAndGetUserTimes --------------------------------------------------------
.checkAndGetUserTimes <- function
(
  times, 
  owConfiguration, 
  ### list with elements \emph{wtaqConfiguration}, \emph{wellfield}, 
  ### \emph{observation} (= data frame containing times and drawdowns)
  activeWell  
) 
{
  return (times)
  
    # If times are given, return these times, otherwise check the WTAQ
    # times configuration if user times are to be used
    if (! is.null(times)) {
      userTimes <- data.frame(t = times)
    }
    else if (owConfiguration$wtaqConfiguration$times$its == 1) { # user-specified times?
      
      # Stop if there are no user-specified times
      userTimes <- owConfiguration$observation[[activeWell]]
      
      if (isNullOrEmpty(userTimes)) {
        stop("*** According to the WTAQ configuration user-specified times are to be ",
             "used but there were\nno times found in owConfiguration$observation for well ",
             owConfiguration$wellfield$wellName[activeWell], 
             "!\nYou may want to set the times argument of this function!")
      }
    }
    else { # log-cycle time steps
      userTimes <- NULL
    } 
    
    userTimes  
  ### data frame with column \emph{t}
}

# owConfigurationToWtConfiguration ---------------------------------------------
owConfigurationToWtConfiguration <- function # WTAQ configuration from Optiwells configuration
### convert Optiwells configuration to WTAQ configuration with one well
### being selected as pumping well
(
  owConfiguration, 
  ### Optiwells configuration as created with \code{\link{owConfigure}}
  activeWell, 
  ### number of active well = row number in \emph{owConfiguration$wellfield}
  Q,
  ### discharge of active well
  times = NULL,
  ### vector of times for which drawdowns are to be calculated    
  solution = wtConfigureSolution()
  ### List of solution parameters as retrieved by
  ### \code{\link{wtConfigureSolution}}.
) 
{
  ##seealso<< \code{\link{wtConfigurationToOwConfiguration}}  
  
  # Wellfield configuration
  wellfield <- owConfiguration$wellfield
  
  # distances between non-active wells and active well
  distances <- owWellDistances(wellfield, activeWell)

  # prepare data frame with "user" times
  userTimes <- data.frame(t = times)
  
  # Create list of observation wells
  observationWells <- list()
  
  # Loop through the wells in the wellfield
  for (i in 1:nrow(wellfield)) {

    if (i == activeWell) {
      
      # Create pumping well configuration
      pumpingWell <- .wellfieldWellToPumpingWell(
        wellfieldWell = wellfield[activeWell, ], 
        drawdownTimeseries = userTimes,
        discharge = Q)
    }        
    else { 
      
      # Create observation well configuration      
      observationWell <- .wellfieldWellToObservationWell(
        wellfieldWell = wellfield[i, ], 
        drawdownTimeseries = userTimes, 
        distanceToPumpingWell = distances[i])
      
      observationWells[[length(observationWells) + 1]] <- observationWell
    }
  }
  
  # Start with WTAQ basic configuration as given in Optiwells configuration
  wtConfigure(
    general = wtConfigureGeneral(
      title = "Configuration generated by R-package kwb.wtaq"),
    aquifer = owConfiguration$aquifer,
    drainage = owConfiguration$drainage,
    times = wtConfigureTimes(its = 1, tlast = 0, nlc = 0, nox = 0),
    solution = solution,
    pumpwell = pumpingWell, 
    obswells = observationWells)
  
  ### WTAQ configuration as e.g. returned by \code{\link{wtConfigure}}
}

# .wellfieldWellToPumpingWell --------------------------------------------------
.wellfieldWellToPumpingWell <- function
(
  wellfieldWell, drawdownTimeseries, discharge
)
{
  wtConfigurePumpwell(
    irun  = 1, 
    ipws  = 0, 
    ipwd  = 1, 
    ipump = 1, 
    qq    = discharge, 
    rw    = wellfieldWell$r, 
    rc    = wellfieldWell$r, 
    zpd   = wellfieldWell$z1, 
    zpl   = wellfieldWell$z2, 
    sw    = wellfieldWell$sw, 
    tspw  = drawdownTimeseries,
    pwname = wellfieldWell$wellName)
}

# .wellfieldWellToObservationWell ----------------------------------------------
.wellfieldWellToObservationWell <- function
(
  wellfieldWell, drawdownTimeseries, distanceToPumpingWell
)
{
  wtConfigureObservationWell(
    irun   = 1, 
    obname = wellfieldWell$wellName,
    iows   = 0, 
    idpr   = 1, 
    r      = distanceToPumpingWell, 
    z1     = wellfieldWell$z1, 
    z2     = wellfieldWell$z2, 
    zp     = 0, 
    rp     = wellfieldWell$r, 
    xll    = wellfieldWell$z2 - wellfieldWell$z1, 
    tsobs  = drawdownTimeseries)
}

# .pumpingWellToWellfieldWell --------------------------------------------------
.pumpingWellToWellfieldWell <- function
(
  pumpingWell, x, y
)
{
  owConfigureWell(
    wellName = pumpingWell$pwname, 
    x = x, 
    y = y, 
    r = pumpingWell$rw, 
    z1 = pumpingWell$zpd, 
    z2 = pumpingWell$zpl, 
    sw = pumpingWell$sw)
}

# .observationWellToWellfieldWell ----------------------------------------------
.observationWellToWellfieldWell <- function
(
  observationWell, x, y, sw
)
{
  owConfigureWell(
    wellName = observationWell$obname, 
    x = x, 
    y = y, 
    r = observationWell$rp, 
    z1 = observationWell$z1, 
    z2 = observationWell$z2, 
    sw = sw)
}

# # owAddDrawdownLines -----------------------------------------------------------
# owAddDrawdownLines <- function
# ### adds lines representing drawdowns to an existing plot
# (
#   wtaqResult
#   ### WTAQ result as e.g. retrieved by \code{\link{wtRunConfiguration}}.
# )
# {
#   if (! ("wtaqResult" %in% class(wtaqResult))) {
#     stop("wtaqResult must be of class wtaqResult!")
#   }  
#   
#   .owAddDrawdownLines()
# }

# .owAddDrawdownLines ----------------------------------------------------------
.owAddDrawdownLines <- function # add drawdown lines to plot
### Add drawdown lines to plot
(
  wres, 
  ### WTAQ result as e.g. retrieved by \code{\link{wtRunConfiguration}}.
  xpos, # wdist, 
  ### x positions of drawdowns 
  myTime, 
  ### vector of times
  refLevel = 0, 
  ### reference level (top of aquifer)
  col = grey((1:(length(myTime)))/(length(myTime)+1)), 
  ilabel = 1,
  ### index of well at which time labels shall appear
  ...
)
{
  # Recycle colour vector to desired length
  col = rep(col, length.out=length(myTime))
  
  i <- 1
  for (ti in myTime) {
    
    # get row out of wres (exclude time!)
    resr <- wres[wres$TIME == ti, -1]
    
    # order x positions
    #ord <- order(wdist)
    ord <- order(xpos)
    
    # Add lines
    lines(xpos[ord], refLevel - resr[ord], col = col[i], ...)

    # Add labels indicating the time
    offset <- 0 # 0.01*diff(par()$usr[3:4])
    text(xpos[ilabel], refLevel-resr[ilabel]-offset, sprintf("t=%s", ti))    

    i <- i+1
  }
}

# owGetDrawdowns ---------------------------------------------------------------
owGetDrawdowns <- function # Drawdowns for Optiwells configuration and pump discharges
### Calculate drawdowns for given Optiwells configuration and pump discharges
(
  owConfiguration, 
  ### Optiwells configuration as e.g. retrieved by \code{\link{owRandomConfiguration}}.
  Q,
  ### vector of discharges at the wells in the order of wells in 
  ### owConfiguration$wellfield
  times = NULL,
  ### vector of times for which drawdowns are to be calculated
  solution = NULL,
  ### List of solution parameters as retrieved by 
  ### \code{\link{wtConfigureSolution}}. If not specified, a default 
  ### configuration, as retrieved by \code{\link{wtDefaultConfigurationSolution}} 
  ### is used.
  to.matrix = TRUE,
  ### if TRUE, the results returned by \code{\link{wtRunConfiguration}} that 
  ### come in "long" form (the drawdowns for each well appear in blocks one
  ### below each other with a column "WELL" indicating the well name) are
  ### converted to "matrix" (= wide) form in which the calculated drawdowns
  ### appear in columns one beside the other.
  ... 
  ### additional arguments passed to \code{\link{wtRunConfiguration}}, such as
  ### \emph{show.results} or \emph{dbg}, see there.
)
{
  ##seealso<< \code{\link{owSuperposeDrawdowns}}    
  
  # If there are not exactly as many discharge values as there are wells, 
  # give a warning but nevertheless recycle the Q vector to as many values
  # as required
  n <- nrow(owConfiguration$wellfield)
  wellNames <- owConfiguration$wellfield$wellName
  
  if (length(Q) != n) {
    warning(sprintf("Length of Q (%d) does not equal number of wells (%d)!\n%s",
                    length(Q), n, "The Q values are recycled!"))
    Q <- recycle(Q, n)
  }  
  
  if (is.null(solution)) {
    solution <- wtDefaultConfigurationSolution(
      aqtype = owConfiguration$aquifer$aqtype, 
      idra = owConfiguration$drainage$idra)
  }
  
  # Prepare result list containing drawdowns
  drawdownList <- list()

  # Loop through wells  
  for (i in 1:n) {
    
    ### Only run WTAQ if the well is pumping
    if (Q[i] > 0) {
      
      # Create i-th WTAQ configuration with the i-th well active
      wtaqConfiguration <- owConfigurationToWtConfiguration(
        owConfiguration, 
        activeWell = i, 
        Q = Q[i], 
        times = times,
        solution = solution)
      
      # Run WTAQ with i-th well active and save result to list drawdownList
      wtaqResult <- wtRunConfiguration(
        wtaqConfiguration, 
        fileExtension = as.character(i), 
        ...)
    }
    else {
      wtaqResult <- .zeroDrawdowns(wellNames, times)
    }

    # Cast to "wide" view (package reshape) if required. Restore the 
    # attributes, such as "src", "wtaqConfiguration" (cast removes them!)
    if (to.matrix) {
      
      wtaqResult <- hsRestoreAttributes(
        cast(wtaqResult, TIME ~ WELL, value = "CALCDD"), 
        attributes(wtaqResult))        
    }    
    
    drawdownList[[wellNames[i]]] <- wtaqResult
  }
  
  # Set attribute oconf
  attr(drawdownList, "oconf") <- owConfiguration
  
  drawdownList

  ### list with as many elements as there are wells defined in the Optiwells 
  ### configuration (list element \emph{wellfield} of \emph{owConfiguration}). For each
  ### pumping well (well with Q > 0) the list element at the corresponding
  ### position contains a data frame holding the drawdowns calculated by WTAQ
  ### with that well pumping alone and the other wells being observation wells.
  ### For each non-pumping well (well with Q = 0) the list element will be NULL.
  ### This list can then be used to calculate the superposition of drawdowns.
}

# .zeroDrawdowns ---------------------------------------------------------------
.zeroDrawdowns <- function # wtaqResult-like structure, filled with zeroes
### wtaqResult-like structure, filled with zeroes
(
  wellNames, times
)
{
  numberOfWells <- length(wellNames)
  numberOfTimes <- length(times)
  
  data.frame(TIME = rep(times, numberOfWells), 
             WELL = sort(rep(wellNames, numberOfTimes)), 
             CALCDD = rep(0, numberOfWells * numberOfTimes),
             stringsAsFactors = FALSE)
}

# .owDrawdownListToDataFrame ---------------------------------------------------
.owDrawdownListToDataFrame <- function
### Convert drawdown list to data frame introducing column PW indicating the
### pumping well
(
  drawdownList
)
{
  # Init result data frame
  res <- NULL
  
  # Loop through list elements
  for (i in 1:length(drawdownList)) {    
    # Append to result data frame
    res <- rbind(
      res, data.frame(PW = attributes(drawdownList)$oconf$wellfield$wellName[i], drawdownList[[i]]))
  }
  
  hsRestoreAttributes(res, attributes(drawdownList))
}

# .owPlotDrawdowns.1 -----------------------------------------------------------
.owPlotDrawdowns.1 <- function
(
  drawdownList
)
{
  dd.df <- .owDrawdownListToDataFrame(drawdownList)
  dat <- hsMatrixToListForm(dd.df, 
                            keyFields = names(dd.df)[1:2], 
                            colNamePar = "WELL", 
                            colNameVal = "CALCDD")
  yrng <- range(dat$CALCDD)

  tro <- lattice::xyplot(CALCDD ~ TIME | PW,                   
                data = dat,
                groups = dat$WELL,
                ylim = rev(yrng + c(-1,1) * 0.04 * diff(yrng)),
                type = c("b", "g"), 
                strip = strip.custom(
                  factor.levels = 
                    sprintf("Pumping well: %s", 
                            attributes(drawdownList)$oconf$wellfield$wellName)),
                auto.key = TRUE, 
                as.table = TRUE
  )
  print(tro)
}

# .owPlotDrawdowns.2 -----------------------------------------------------------
.owPlotDrawdowns.2 <- function
(
  dd
)
{
  n <- length(dd)
  att <- attributes(dd)
  cols <- rainbow(n)
  
  # Plot into matrix of two rows and one column
  opar <- par(mfrow = c(2, 1))    
  
  wellfield <- att$oconf$wellfield
  
  # Loop through list elements
  for (i in 1:n) {
    
    # 1st plot: top view on well field with well i as reference well
    #.owPlotTopView(wellfield, col = cols, ref = i)
    owPlotConfiguration(att$oconf, referenceWell = i, view = "top")
    
    # 2nd plot: side view on well field    
    #wtaqConfiguration <- attributes(dd[[i]])$wtaqConfiguration    
    #owPlotConfiguration(att$oconf, referenceWell = i, view = "top")#, xpos=xpos, xlim=xlim) #xpos=wf$x
    wtPlotResult(dd[[i]], plottype = "d", pumpingWellName = wellfield$wellName[i])
  }
  
  # Reset graphical parameters
  par(opar)  
}

# owPlotDrawdowns --------------------------------------------------------------
owPlotDrawdowns <- function # plot drawdown (under construction!)
### plot drawdown (under construction!)
(
  drawdownList, 
  topview = TRUE, 
  overlay = FALSE,
  fixview = FALSE
)
{
  # 1st plot: drawdown over time for each well being the pumping well
  .owPlotDrawdowns.1(drawdownList)
  
  # 2nd plot: one page per well: drawdown around well being pumping well
  .owPlotDrawdowns.2(drawdownList)
}

# owSuperposeDrawdowns ---------------------------------------------------------
owSuperposeDrawdowns <- function # superpose drawdowns
### superpose drawdowns in list of drawdowns as returned by
### \code{\link{owGetDrawdowns}}.
(
  drawdownList,
  ### list of drawdowns as returned by \code{\link{owGetDrawdowns}}
  dbg = FALSE
  ### if TRUE, debug messages are shown. Default: FALSE  
) 
{
  ##seealso<< \code{\link{owGetDrawdowns}}      
  
  # Number of elements in the list of WTAQ results
  n <- length(drawdownList)

  # Init result data frame
  result <- NULL
  
  for (i in 1:n) {
    
    wtaqResult <- drawdownList[[i]]
    
    if (i == 1) {
      result <- wtaqResult      
    } 
    else {      
      result[, -1] <- result[, -1] + wtaqResult[, -1]
    }
  }
  
  result
  ### data frame
}
