# .plotWell --------------------------------------------------------------------
.plotWell <- function # plot a well
### plot a well
(
  x, 
  ### x position of centre of well
  r, 
  ### inside radius of well pipe
  z1, 
  ### top of screen as depth below top of aquifer
  z2, 
  ### bottom of screen as depth below top of aquifer
  z0 = 0, 
  ### ground level
  r2 = r, 
  ### inside radius of screen
  col="black", 
  ### line colour
  lwd = 1
  ### line width
)
{
  # draw borehole (from ground level to screen top)
  rect(x-r, z0, x+r, z1, border = col, lwd = lwd)
  
  # draw screen
  segments(x + r2*c(-1, 1, -1, -1), c(z1, z1, z1, z2), 
           x + r2*c(-1, 1,  1,  1), c(z2, z2, z1, z2),
           lty = c(2,2,1,1), lwd = lwd, col = col)
  
  # If r2 is 0, the screen will not be visible. In this case, draw a point
  # in the middle between z1 and z2
  if (r2 == 0) {
    points(x, (z1+z2)/2, col=col, pch=16)
  }
}

# .owPlotTopView ---------------------------------------------------------------
.owPlotTopView <- function
(
  wellfield, 
  ### data frame with columns \emph{x}, \emph{y}, \emph{r}, \emph{wellName}, as
  ### e.g. retrieved by \code{\link{owRandomWellfield}} with each row
  ### representing a well of the well field.
  refid = -1, 
  ### ID of reference well, i.e. well that is selected as productive well.
  ### If this ID is given, circle lines are draw around the corresponding well
  ### and the x range is extended to the right so that the intersection of the
  ### cirle line of the most distant well keeps visible. If refid is -1 no
  ### well has the focus and no circle lines are drawn.
  col = rainbow(nrow(wellfield)),
  ### colours to be used for the wells. Default: rainbow colours
  main = "Top view on well field"
  ### plot title. Default: "Top view on well field"
) 
{
  # Calculate distances to reference well if reference well is given
  if (refid != -1) {
    wdist <- owWellDistances(wellfield, refid)    
  }
  
  # minimum x coordinate
  iminx <- which.min(wellfield$x)
  xmin <- wellfield$x[iminx] - wellfield$r[iminx]
  
  # maximum x coordinate
  if (refid == -1) {
    imax <- which.max(wellfield$x)
    xmax <- wellfield$x[imax] + wellfield$r[imax]
  } else {
    imax <- which.max(wdist)
    xmax <- wellfield$x[refid] + wdist[imax] + wellfield$r[imax]
  }  
  
  # minimum and maximum y coordinate
  imin <- which.min(wellfield$y)
  imax <- which.max(wellfield$y)
  ymin <- wellfield$y[imin] - wellfield$r[imin]
  ymax <- wellfield$y[imax] + wellfield$r[imax]
  
  # x and y range
  xlim <- c(xmin, xmax)
  ylim <- c(ymin, ymax)
  
  # Set the title
  if (refid != -1) {
    main <- paste(main, ", reference well = ", wellfield$wellName[refid], sep="")
  }
  
  # Prepare the plot area
  plot(NULL, NULL, asp = 1, type = "n", xlim = xlim, ylim = ylim, 
       xlab = "x coordinate", ylab = "y coordinate", main = main)    
  
  # Number of wells
  numberOfWells <- nrow(wellfield)
  
  # Recyle colour vector to the required length
  col <- recycle(col, numberOfWells)
  
  # If a reference well is given, draw connecting lines from the reference well 
  # to each other well and circle lines around the reference well with radiuses
  # corresponding to the distances to the other wells
  if (refid != -1) {
    
    # Draw connecting lines
    segments(wellfield$x[ refid], wellfield$y[ refid], 
             wellfield$x[-refid], wellfield$y[-refid], lty = 2)
    
    # Draw horizontal reference line 
    abline(h = wellfield$y[refid], col = "grey")    
    
    # Draw vertical lines through the centre of the reference well and in
    # distances corresponding to the well distances
    abline(v=wellfield$x[refid] + wdist, col="grey", lty=2)
    
    # Draw radiuses around reference well
    cx <- wellfield$x[refid] # x/y coordinate of circle centre
    cy <- wellfield$y[refid]    
    for (i in (1:numberOfWells)[-refid]) {
      draw.circle(cx, cy, radius = wdist[i], border = col[i])
    }
  } 
  else {
    # Draw horizontal and vertical lines at the x and y coordinats of the
    # well centres
    abline(h=wellfield$y, v = wellfield$x, col = "grey", lty = 2)
  }
  
  # Draw the wells
  for (i in 1:numberOfWells) {
    draw.circle(wellfield$x[i], wellfield$y[i], wellfield$r[i], col = col[i])
  }
  
  # Label the wells
  text(wellfield$x, wellfield$y, wellfield$wellName)
  
  # return x limits
  return(par()$usr[1:2])
}

# .plotSideView ----------------------------------------------------------------
.plotSideView <- function # plot side view of wellfield
### Plot side view of wellfield
(
  x, 
  ### vector of x positions at which wells are to be plotted. 
  r,
  ### vector of radiuses of wells in units of length
  z1,
  ### begin of screen (depth below top of aquifer) of wells in units of length
  z2,
  ### end of screen (depth below top of aquifer) of wells in units of length
  r2 = r,
  ### vector of radiuses of well screen (equals r by default)
  thick.unsat = 0,
  ### thickness of unsaturated zone in units of length
  thick.aquif = max(1.05*z2),
  ### thickness of aquifer in units of length
  col = rainbow(length(x)),
  ### colours by which wells are to be represented
  name = sprintf("W%02d", 1:length(x)),
  ### vector of well names
  xlab = "",
  ### x-axis label
  main = "Side view on well field",
  legend.cex = 1,
  ### character expansion factor for legend
  wellaxes = TRUE,
  ### if TRUE, a vertical line representing the well axis is drawn for each well
  bottom.aquif = TRUE,
  ### if TRUE, plot is scaled so that bottom of aquifer is visible. Default: TRUE
  col.aquif = "lightcyan", #"powderblue" # "lightblue1" # "steelblue1"
  ### colour of aquifer
  col.unsat = "wheat", # "tan",
  ### colour of unsaturated zone
  legend.mar = 5,  
  ### margin (in number of lines) on the right side to be reserved for legend
  asp = 1,
  ### aspect ratio between x and y axis. Default: 1. Set to NA if aspect ratio
  ### does not matter.
  xlim = NULL,
  ylim = NULL,
  ...
  ### further arguments passed to .plotWell
) 
{
  # Recycle all vectors to length of x
  n  <- length(x)
  
  r   <- recycle(r, n)
  r2  <- recycle(r2, n)
  z1  <- recycle(z1, n)
  z2  <- recycle(z2, n)
  col <- recycle(col, n)
  
  # Expand margin to make room for legend (reset graphic parameters on exit)
  opar <- par(mar=par()$mar + c(0, 0, 0, legend.mar))    
  on.exit(par(opar))    
  
  # Prepare plot area
  imin <- which.min(x)
  imax <- which.max(x)

  ymax <- ifelse(bottom.aquif, thick.aquif, max(z2))

  if (is.null(xlim)) {
    xlim <- c(x[imin] - max(r[imin], r2[imin]), 
              x[imax] + max(r[imax], r2[imax]))
  }

  if (is.null(ylim)) {
    ylim <- c(ymax, -thick.unsat)
  }  
  
  plot(NA, NA, # xaxs <- "r" # regular, xaxs <- "i" # internal
       xlim = xlim,
       ylim = ylim,
       asp = asp,
       xlab = xlab, 
       ylab = "depth below top of aquifer (m)",
       main = main)
  
  # get coordinates of plot area
  usr <- par()$usr
  
  # draw unsaturated zone(s)
  abline(h = -thick.unsat, lwd=2, col=col.unsat)

  rect(usr[1], -thick.unsat, usr[2], 0, angle = -45, #density = 15,
       lwd = 0.5, col = col.unsat)
  
  rect(usr[1], thick.aquif, usr[2], usr[3], angle = -45, #density = 15,
       lwd = 0.5, col = col.unsat)
  
  # draw aquifer body
  rect(usr[1], 0, usr[2], min(usr[3], thick.aquif), lwd = 0.5, 
       col = col.aquif)#, density = 5)
  
  # plot wells  
  for (i in 1:length(x)) {
    .plotWell(x[i], r[i], z1[i], z2[i], r2 = r2[i], z0 = -thick.unsat, 
              col = col[i], ...) 
  }
  
  # plot vertical lines representing axes of observation wells
  if (wellaxes) {
    abline(v = x, col = col, lty = "twodash", lwd = 0.8) # x[-1]?
  }
  
  # draw legend
  legend("left", lty = "solid", box.lty = 0, border="black",
         legend = name, col = col, cex = legend.cex, inset = 1.01, xpd = TRUE) 
}

# .wtAddXCoordToResult ---------------------------------------------------------
.wtAddXCoordToResult <- function # Add well's x coordinate to WTAQ result
### Add column containing x coordinate of well to WTAQ result. It is expected
### that wtaqResult already is in "list form".
(
  wtaqResult, 
  wtaqConfiguration
) 
{
  # Add a column x representing the distance to the pumped well

  distances <- wtConfiguredDistances(wtaqConfiguration)
  merge(wtaqResult, data.frame(WELL = names(distances), x = distances))  
}

# .wtAddDrawdownLines ----------------------------------------------------------
.wtAddDrawdownLines <- function
(
  wtaqResult, 
  ### WTAQ result as returned by \code{\link{wtRunConfiguration}}
  value = "CALCDD", 
  ### name of value column
  times = sort(unique(wtaqResult$TIME)),
  ### Vector of times to plot. Default: sorted unique values in column TIME 
  ### of wtaqResult.
  col = NULL,
  dbg = FALSE,
  ...
  ### additional parameters given to lines
) 
{
  wtaqConfiguration <- attr(wtaqResult, "wtaqConfiguration")

  # Add column x (distance to pumping well)
  wtaqResultAndX <- .wtAddXCoordToResult(wtaqResult, wtaqConfiguration)
  
  checkForMissingColumns(wtaqResultAndX, c("x", "TIME"))
  
  # Order result data frame by x and t
  row.order <- order(wtaqResultAndX$x, wtaqResultAndX$TIME)
  wtaqResultAndX <- wtaqResultAndX[row.order, ]
  
  # number of simulated times
  n <- length(times)
  
  # Give default colour vector
  col <- .getColours(col = col, n = n)
  
  # Loop through times
  for (i in 1:n) {
    
    # Filter for given time
    sdat <- wtaqResultAndX[wtaqResultAndX$TIME == times[i], , drop=FALSE]
    
    if (dbg) {
      if (nrow(sdat) == 0) {
        warning("no data for time: ", times[i])
      }
      else {
        cat("sdat:\n");print(sdat)  
      }      
    }
        
    # Draw line
    lines(sdat$x, sdat[[value]], col=col[i], ...)
  }  
}

# .getColours ------------------------------------------------------------------
.getColours <- function(col, n)
{
  if (is.null(col)) {
    rev(grey(0:(n-1)/n))
  } 
  else {
    recycle(col, n)
  }  
}

# .wtPlotResultOverX -----------------------------------------------------------
.wtPlotResultOverX <- function # plot drawdowns along x coordinates of wells
### plot drawdowns along x coordinates of wells
(
  wtaqResult, 
  ### WTAQ result
  wtaqConfiguration = attr(wtaqResult, "wtaqConfiguration")
  ### WTAQ configuration
)
{
  # Add x coordinate to the result data frame
  wtaqResultAndX <- .wtAddXCoordToResult(wtaqResult, wtaqConfiguration)
  
  # Order by x
  wtaqResultAndX <- wtaqResultAndX[order(wtaqResultAndX$x), ]
  
  # Prepare colours
  times <- unique(wtaqResultAndX$TIME)
  cols <- rainbow(length(times))
  
  # Plot the drawdown over time at the pumped well
  trellisObject <- xyplot(
    wtaqConfiguration$aquifer$bb - DD ~ x, 
    groups = as.factor(wtaqResultAndX$TIME), 
    data = wtaqResultAndX,  
                type = "b",
                col = cols, distribute.col = TRUE, 
                xlab = "Distance from pumping well",
                ylab = "Drawdown", 
                main = "Evolution of drawdown over time",
                legend = list(right = list(fun = draw.colorkey, 
                                           args = list(key = list(
                                             col = cols, 
                                             at = log(times, 10))))))
  
  print(trellisObject)
}

# .wtPlotResult.1 --------------------------------------------------------------
.wtPlotResult.1 <- function 
(
  wtaqResult, 
  main = "Drawdowns simulated by WTAQ", 
  ylim = NULL,
  meas = ("MEASDD" %in% names(wtaqResult)), 
  timeColumn = "TIME", 
  auto.key = NULL,
  ...
)
{
  wtaqConfiguration <- attr(wtaqResult, "wtaqConfiguration")
  
  auto.key <- .defaultAutoKeyIfNull(auto.key, wtaqConfiguration)
  
  ylim <- .defaultYlimIfNull(ylim, wtaqResult, meas)
  
  trellisObject <- xyplot(
    as.formula(sprintf(
      "CALCDD %s~ %s", ifelse (meas, "+ MEASDD ", ""), timeColumn)), 
    data = wtaqResult, 
    groups = wtaqResult$WELL, 
    ylim = ylim,
    type = c("b", "g"),
    main = main, 
    auto.key = auto.key, ...)

  print(trellisObject)
}

# .defaultAutoKeyIfNull --------------------------------------------------------
.defaultAutoKeyIfNull <- function (auto.key, wtaqConfiguration)
{
  if (is.null(auto.key)) {
    list(columns = length(wtConfiguredWellnames(wtaqConfiguration)))
  }
  else {
    auto.key    
  }
}
  

# .defaultYlimIfNull -----------------------------------------------------------
.defaultYlimIfNull <- function(ylim, wtaqResult, measurements)
{
  if (is.null(ylim)) {
    values <- wtaqResult$CALCDD
    if (measurements) {
      values <- c(values, wtaqResult$MEASDD)
    }
    rev(extendLimits(range(values), 0.04))
  }
  else {
    ylim
  }
}

# .wtPlotResult.2 --------------------------------------------------------------
.wtPlotResult.2 <- function 
(
  wtaqResult, 
  main = "Drawdowns simulated by WTAQ", 
  ylim = NULL, 
  meas = ("MEASDD" %in% names(wtaqResult)), 
  timeColumn = "TIME",
  auto.key = NULL, 
  ...
)
{
  wtaqConfiguration <- attr(wtaqResult, "wtaqConfiguration")

  ylim <- .defaultYlimIfNull(ylim, wtaqResult, meas)
  
  if (meas) {    
    
    mdata <- hsMatrixToListForm(
      hsRenameColumns(removeColumns(
        wtaqResult, "RELERR"), 
        list(CALCDD = "calculated", MEASDD = "measured")),
      keyFields = c("WELL", timeColumn),
      colNamePar = "Type",
      colNameVal = "DD")

    groups <- mdata$Type

    ylab <- "Drawdown in units of length"
    if (is.null(auto.key)) {
      auto.key <- list(columns = 2)
    }
  } 
  else {
    mdata <- wtaqResult
    groups <- NULL  

    ylab <- "Calculated drawdown in units of length"    
    if (is.null(auto.key)) {
      auto.key <- FALSE
    }
  }
  
  frm <- sprintf("%s ~ %s | WELL", ifelse(meas, "DD", "CALCDD"), timeColumn)

  trellisObject <- xyplot(
    as.formula(frm), 
    data = mdata, 
    groups = groups,
    ylim = ylim,
    ylab = ylab,
    type = c("b", "g"),
    main = main, 
    auto.key = auto.key, ...)      
  
  print(trellisObject)
}

# .wtPlotResult.3 --------------------------------------------------------------
.wtPlotResult.3 <- function 
(
  wtaqResult, 
  meas = ("MEASDD" %in% names(wtaqResult)), 
  asp = 1, 
  col = NULL,
  dbg = FALSE
)
{
  if (dbg) {
    cat("wtaqResult in .wtPlotResult.3:\n")
    print(wtaqResult)    
  }

  wtaqConfiguration <- attr(wtaqResult, "wtaqConfiguration")
  
  if (is.null(col)) {
    col <- rainbow(1 + length(wtaqConfiguration$obswells))    
  }
  
  # Plot well configuration 
  .wtPlotConfigurationAndDrawdowns(
    wtaqConfiguration, wtaqResult, valueColumn = "CALCDD", 
    main = "Calculated drawdowns", col = col, asp = asp)
  
  if (meas) {
    .wtPlotConfigurationAndDrawdowns(
      wtaqConfiguration, wtaqResult, valueColumn = "MEASDD", 
      main = "Measured drawdowns", col = col, asp = asp)
  }
}

# .wtPlotConfigurationAndDrawdowns ---------------------------------------------
.wtPlotConfigurationAndDrawdowns <- function
(
  wtaqConfiguration, wtaqResult, valueColumn, main, col, ...
)
{
  # first colour is colour for pumping well, remaining colours are colours
  # for observation wells
  wtPlotConfiguration(wtaqConfiguration, main = main,  
                      col.pw = col[1], col.ow = col[-1], ...)
  
  .wtAddDrawdownLines(wtaqResult, value = valueColumn)
}

# .wtPlotResult.4 --------------------------------------------------------------
.wtPlotResult.4 <- function 
(
  wtaqResult, 
  timeColumn = "TIME", 
  meas = ("MEASDD" %in% names(wtaqResult)), 
  asp = 1, 
  PNG = FALSE
)
{
  wtaqConfiguration <- attr(wtaqResult, "wtaqConfiguration")
  
  if (PNG) {    
    pngDirectory <- tempSubdirectory("wtaqResult")
    
    # Delete existing png files
    file.remove(dir(pngDirectory, "\\.png$", full.names=TRUE))  
    
    # Base path and name for png files
    pngFilenameBase <- file.path(pngDirectory, "wtaqResult")
  }
  
  # Calculated (+ measured, if applicable) drawdown for each time
  i <- 1
  for (ti in unique(wtaqResult[[timeColumn]])) {

    if (PNG) {
      pngFile <- sprintf("%s_%04d.png", pngFilenameBase, i)
    }
    else {
      pngFile <- NULL
    }
    
    .wtPlotDrawdownTimeseries(wtaqResult, ti, meas, asp, pngFile = pngFile)

    i <- i + 1
  }
  
  if (PNG) {
    return(pngFile)
  }
}

# .wtPlotDrawdownTimeseries ----------------------------------------------------
.wtPlotDrawdownTimeseries <- function
(
  wtaqResult,
  ti,
  meas, 
  asp,
  pngFile = NULL,
  wtaqConfiguration = attr(wtaqResult, "wtaqConfiguration"),  
  main = sprintf("Calculated vs Measured drawdown at t = %0.0f", ti)
)
{
  if (!is.null(pngFile)) {
    png(pngFile)
    on.exit(dev.off())
  }
  
  wtPlotConfiguration(wtaqConfiguration, main = main, asp = asp)
  
  .wtAddDrawdownLines(
    wtaqResult, value = "CALCDD", times = ti, col = "blue")
  
  if (meas) {
    
    .wtAddDrawdownLines(
      wtaqResult, value = "MEASDD", times = ti, 
      col = "red", type = "p", pch = 15)
    
    legend("top", 
           legend = c("calculated", "measured"), 
           border = FALSE,
           col = c("blue", "red"), 
           lty = c(1, 0), 
           pch = c(NA, 15))
  }  
}
