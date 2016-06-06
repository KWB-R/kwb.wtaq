# wtPlotResult -----------------------------------------------------------------
wtPlotResult <- function # Plot WTAQ results
### Plot WTAQ results
(
  wtaqResult, 
  ### data frame as returned by \code{\link{wtRunConfiguration}}
  main = "",
  ### plot title, default: ""
  plottype = "w",
  ### vector of plot types ("s" = superposed, "w" = one plot per well,
  ### "d" = along distance to pump well, "t" each time along distance to well).
  ### Default: "w" (one plot per well).
  showMeasurements = TRUE,
  ### if TRUE, measurements are shown
  auto.key = NULL,
  ### given to xyplot, see there. If NULL, a default key with as many columns as
  ### there are wells is used. Default: NULL.
  asp  = NA,
  ### aspect ratio between x and y axis. Default: 1. Set to NA if aspect ratio
  ### does not matter.  
  PDF = FALSE,
  ### if TRUE, a pdf file is created in tempdir() and opened in a PDF viewer
  PNG = FALSE,
  ### if TRUE, all plots made with plot type "t" are saved to png files in
  ### tempdir()/wtaqResult.
  pumpingWellName = "PW",
  ### name of pumping well in wtaqResult
  xlim = NULL,
  ylim = NULL,
  ...
  ### additional arguments given to xyplot
)
{
  att <- attributes(wtaqResult)  
  
  if (main == "") {
    main <- paste("WTAQ result read from plot-file:", att$src)
  }
  
  # Name of time column
  columnNames <- names(wtaqResult)
  timeColumn <- ifelse("TIME" %in% columnNames, "TIME", "TDRDSQ")  
  
  # Convert to "list form" if in "matrix form"
  if (! ("WELL" %in% names(wtaqResult))) {
    wtaqResult <- hsMatrixToListForm(
      wtaqResult, 
      keyFields = timeColumn, 
      colNamePar = "WELL", colNameVal="CALCDD")
    
    wtaqResult <- hsRestoreAttributes(wtaqResult, att)      
  }
  
  # Measurements available and to be shown?
  meas <- "MEASDD" %in% columnNames && showMeasurements
  
  if (PDF) {
    ON.EXIT <- function(pdff){
      dev.off()
      hsShowPdf(pdff)
    }
    pdff <- hsPrepPdf()    
    on.exit(ON.EXIT(pdff))    
  }
  
  # All calculated drawdowns over time in one plot, and, if available, 
  # all measured drawdowns over time in another plot
  if ("s" %in% plottype) { 
    .wtPlotResult.1(
      wtaqResult = wtaqResult, 
      main = main, 
      ylim = ylim, 
      meas = meas, 
      timeColumn = timeColumn, 
      auto.key = auto.key, 
      ...)
  }
  
  # One plot per well
  if ("w" %in% plottype) { 
    .wtPlotResult.2(
      wtaqResult = wtaqResult,
      main = main, 
      ylim = ylim, 
      meas = meas, 
      timeColumn = timeColumn, 
      auto.key = auto.key, 
      ...)
  }
  
  # Get WTAQ configuration from attribute "wtaqConfiguration"
  wtc <- att$wtaqConfiguration

  if (is.null(wtc)) {
    warning("WTAQ result data does not have an attribute 'wtaqConfiguration' ",
            "containing the WTAQ configuration from which the result data ",
            "was generated.")
  }
  else {
    
    # Drawdown along distance to pumping well: all times in one plot
    if ("d" %in% plottype) {
      .wtPlotResult.3(
        wtaqResult = wtaqResult, 
        meas = meas, 
        asp = asp)
    }
    
    # Drawdown along distance to pumping well: One plot per time
    if ("t" %in% plottype) {
      pngFile <- .wtPlotResult.4(
        wtaqResult = wtaqResult, 
        timeColumn = timeColumn, 
        meas = meas, 
        asp = asp, 
        PNG = PNG)
    }    
  }
  
  if (PNG) {
    return(pngFile)
  }
}

# wtPlotConfiguration ----------------------------------------------------------
wtPlotConfiguration <- function # plot wellfield profile
### plot wellfield profile
(
  configuration = wtConfigurationExample3(), 
  ### configuration as retrieved by \code{\link{wtConfigure}}, 
  ### default: wtConfigurationExample3()
  col.pw = "black",
  ### pumping well colour, default: "black"
  col.ow = rainbow(length(configuration$obswells)),  
  ### observation well colours. Default: rainbow colours
  main = "WTAQ Configuration with Pumping Well on the Left",
  ### title for the plot
  asp = 1,
  ### aspect ratio, see ?plot  
  ...
  ### further arguments passed to .plotSideView
) 
{
  # shortcuts to pumpwell/obswells setting  
  pumpingWell <- configuration$pumpwell
  observationWells <- configuration$obswells
  
  x  <- 0
  r  <- pumpingWell$rc 
  z1 <- pumpingWell$zpd
  z2 <- pumpingWell$zpl

  getZ <- function(ow, ztype) {    
    
    # z1 (ztype == 1) or z2 requested?
    sig <- ifelse(ztype == 1, -1, 1)
    
    # Piezometer?
    ifelse(ow$iows == 2, 
           ow$zp + sig * ow$xll / 2, 
           ifelse(sig == -1, ow$z1, ow$z2))
  }
  
  if (length(observationWells) > 0) {
    x  <- c(x,  sapply(observationWells, FUN = "[[", "r"))    
    r  <- c(r,  sapply(observationWells, FUN = "[[", "rp"))
    z1 <- c(z1, sapply(observationWells, FUN = getZ, 1))
    z2 <- c(z2, sapply(observationWells, FUN = getZ, 2))
  }

  # recyle vector of observation well colours
  col.ow <- recycle(col.ow, length(observationWells))
  
  # Set screen radius to well radius for observation wells
  r2 <- c(pumpingWell$rw, r[-1])

  .plotSideView(
    x, r, z1, z2, r2 = r2, 
    thick.aquif = configuration$aquifer$bb, 
    col = c(col.pw, col.ow), 
    name = wtConfiguredWellnames(configuration), 
    xlab = "Distance to Pumping Well (m)", 
    main = main,
    asp = asp,
    ...)
}

# owPlotConfiguration ----------------------------------------------------------
owPlotConfiguration <- function # Plot Optiwells configuration
### Plot Optiwells configuration
(
  owConfiguration, 
  ### Optiwells configuration as retrieved by \code{\link{owConfigure}}
  referenceWell = -1, 
  ### Number of reference well (according to row number in
  ### owConfiguration$wellfield or -1 (no reference well). If a reference well
  ### is specified, it will be plotted on the left-hand side of the sideview and
  ### the top view will be centered around that well.
  view = c("top", "side"), 
  ### vector of c("top", "side"). Determines whether to plot top view or side
  ### view or both.
  col = rainbow(nrow(owConfiguration$wellfield)),
  ### colours. Default: rainbow colours
  onePage = TRUE,
  ### if TRUE, top view and side view appear below each other on one and the
  ### same page
  ...
)
{
  # wellfield configuration
  wellfield <- owConfiguration$wellfield
  
  # Prepare plot matrix of two rows and one column if onePage = TRUE
  if (onePage && length(intersect(view, c("top", "side"))) == 2) {
    opar <- par(mfrow = c(2, 1))
    on.exit(par(opar))
  }
  
  if ("top" %in% view) {
    .owPlotTopView(wellfield, refid = referenceWell)
  }
  if ("side" %in% view) {
    main <- "Side view on well field"
    if (referenceWell == -1) {
      x <- wellfield$x
      ord <- 1:length(x)
      xlab = "x coordinate"      
    } 
    else {
      x <- owWellDistances(wellfield, referenceWell = referenceWell)    
      ord <- order(x)
      xlab = sprintf("Distance to reference well %s", wellfield$wellName[referenceWell])
      main <- paste(main, ", reference well = ", wellfield$wellName[referenceWell], sep="")
    }
    
    .plotSideView(x[ord], wellfield$r[ord], wellfield$z1[ord], wellfield$z2[ord], 
                  col = col[ord], name = wellfield$wellName[ord], 
                  main=main, xlab = xlab, ...)
    
    #   # draw arrows indicating discharges
    #   ytop <- bb - pw$zpd
    #   if (pw$qq > 0 && qhRatio > 0) {
    #     arrows(r, ytop, r, ytop+qhRatio*pw$qq, lwd = 2, col = "red")    
    #   } 
  }
}
