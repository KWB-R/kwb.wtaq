## ------------------------------------------------------------------------
## package 'zoo' successfully unpacked and MD5 sums checked
## package 'hydroGOF' successfully unpacked and MD5 sums checked
## package 'lattice' successfully unpacked and MD5 sums checked
## package 'plotrix' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
##  C:\Dokumente und Einstellungen\Key Hauke\Lokale Einstellungen\Temp\RtmpctBYMz\downloaded_packages
## package 'kwb.utils' successfully unpacked and MD5 sums checked

## ----echo = FALSE--------------------------------------------------------
#currentWorkingDirectory <- getwd()
#setwd("installation")
#source(file.path("installPackages.R"))
#setwd(currentWorkingDirectory)

## ---- warning = FALSE, message = TRUE------------------------------------
library(kwb.wtaq)

# ### Download WTAQ 2.1 from USGS
# download.file("https://water.usgs.gov/ogw/wtaq/WTAQ_2.1.exe", 
#               destfile =  system.file("extdata/wtaq.2.1.exe", 
#                                       package = "kwb.wtaq"))


## ------------------------------------------------------------------------
## Warning: package 'kwb.utils' was built under R version 3.0.3

## ---- eval = FALSE-------------------------------------------------------
#  ?kwb.wtaq::wtConfigure
#  help(kwb.wtaq::wtConfigure)

## ----echo = FALSE--------------------------------------------------------
#Als Grundlage w?re ein gutes Beispielmodell mit guten Beispieldaten (Messdaten #f?r Kalibrierung) wichtig, das wir durchg?ngig verwenden k?nnen und das wir mit #in das Paket packen mit Zugriffsfunktionen, z.B. getTutorialData(), getTutorial#Model()

## ------------------------------------------------------------------------
data("drawdowns", package = "kwb.wtaq")

## ------------------------------------------------------------------------
str(drawdowns)

## ------------------------------------------------------------------------
drawdowns[["W5"]]

## ----invisible = TRUE, asis = TRUE---------------------------------------
xyplot(W1 + W2 + W3 + W4 + W5 ~ time.in.seconds, 
       data = drawdowns[["W5"]], 
       type = c("b", "g"), # (b)oth, dots and lines, and a (g)rid
       auto.key = list(columns = 5), # legend arranged in five columns
       ylim=c(1,-0.1),
       ylab= "Drawdown (m)", # label of y-Axis
       xlab= "Time in seconds since start of pumping in W5 (s)") # label of y-Axis

## ------------------------------------------------------------------------

generalConfiguration <- wtConfigureGeneral(
  ### title of the project (max. length 70 characters)
  title="Example well field, long-term pumping test of well 5"
  )

aquiferConfiguration <- wtConfigureAquifer(    
  aqtype = "WATER TABLE", # aquifer type
  bb = 10,                # saturated aquifer thickness
  hkr = 1E-03,            # horizontal hydraulic conductivity
  hkz = 3.5E-05,          # vertical hydraulic conductivity
  ss = 1E-05,             # specific storage
  sy = 0.05               # specific yield
)

drainageConfiguration <- wtConfigureDrainage(
  idra = 0 # = instantaneous drainage in unsaturated zone
)

timesConfiguration <- wtConfigureTimes(
  its = 1 # = user-specified time-steps
)

## ------------------------------------------------------------------------
drawdowns5 <- drawdowns[["W5"]]

## ------------------------------------------------------------------------
# the times of observations are the same for all wells:
times <- drawdowns5$time.in.seconds

# observed drawdowns at the pumping well
observed.PW  <- data.frame(t = times, dd = drawdowns5$W5)

# observed drawdowns at the observation wells
observed.OW1 <- data.frame(t = times, dd = drawdowns5$W1)
observed.OW4 <- data.frame(t = times, dd = drawdowns5$W4)

## ------------------------------------------------------------------------
pumpwellConfiguration <- wtConfigurePumpwell(
  ### partially penetrating pumped well
  ipws = 0,
  ### finite diameter well
  ipwd = 1, 
  ### pumping rate of production well in (here: m3/s)
  qq = 0.0869, 
  ### radius of pumped well-screen (here: meter) 
  rw = 1.5, 
  ### top of filter screen below initial water table (here: meter)
  zpd = 0.4, 
  ### bottom of filter screen below initial water table (here: meter)
  zpl = 7.8, 
  ### well-bore skin parameter (dimensionless)
  sw = 0, 
  ### data.frame with times and measured drawdown data in pumping well
  tspw = observed.PW 
)

## ------------------------------------------------------------------------
observationWell1 <- wtConfigureObservationWell(
  ### name of observation well
  obname = "OW1", 
  ### distance from pumping well (here: meters)
  r = 309.5, 
  ### partially penetrating observation well
  iows = 0, 
  ### delayed response
  idpr = 1, 
  ### top of filter screen below initial water table (here: meters)
  z1 = 1.8, 
  ### bottom of filter screen below initial water table (here: meters)
  z2 = 7.5, 
  ### inside radius of the observation well (here: meters)
  rp = 1.5, 
  ### data.frame with times and measured drawdown data in OW1
  tsobs= observed.OW1 
)

## ------------------------------------------------------------------------
observationWell4 <- wtConfigureObservationWell(obname = "OW4", r = 86.6,
  iows = 0, idpr = 1, z1 = 1.8, z2 = 7.5, rp = 1.5, tsobs = observed.OW4)

## ------------------------------------------------------------------------
wtaqConfiguration <- wtConfigure(
  general = generalConfiguration,
  aquifer = aquiferConfiguration, 
  drainage = drainageConfiguration, 
  times = timesConfiguration, 
  solution = wtConfigureSolution(),
  pumpwell = pumpwellConfiguration,
  obswells = list(observationWell4, 
                  observationWell1)
)

## ---- echo=TRUE----------------------------------------------------------
inputFile <- system.file("extdata", "example1.inp", package = "kwb.wtaq")
wtaqConfiguration2 <- wtReadInputFile(inputFile)

## ---- echo=TRUE----------------------------------------------------------
wtaqConfiguration

## ---- eval=TRUE----------------------------------------------------------
wtPlotConfiguration(wtaqConfiguration, asp = NA)

## ---- eval=TRUE----------------------------------------------------------
result <- wtRunConfiguration(wtaqConfiguration)

## ---- eval=TRUE----------------------------------------------------------
result

## ---- eval=TRUE----------------------------------------------------------
wtPlotResult(result, 
             plottype = "w", 
             main="Pumping test W5: model results without calibration")

## ---- eval=TRUE----------------------------------------------------------
wtPlotResult(result, 
             plottype = "s",
             main="Pumping test W5: model results without calibration")

## ---- eval=TRUE----------------------------------------------------------
# modelFitness(): called by function modelFitnessAggregated()
modelFitness <- function
(
  wtaqResult, 
  wellPattern
  )
  {
  subResult <- wtaqResult[grep(pattern = wellPattern, wtaqResult$WELL),]
  
  
  fitness <- t(gof(sim = subResult$MEASDD, obs = subResult$CALCDD, digits = 3))
  
  colnames(fitness) <- sub(" %", "", colnames(fitness))
  
  ### data.frame with plenty of performance indicators: e.g. RMSE, NSE, R2
  as.data.frame(fitness)
  }

# modelFitnessAggregated: called by function fitnessAdaptedModelConfiguration()
modelFitnessAggregated <- function 
(
  wtaqResult, 
  wellPattern
  )
  {
  fitness <- modelFitness(wtaqResult, wellPattern)
  
  ### Objective function for the performance criteria that is minimised, here:
  fitness$RMSE 
  }

# fitnessAdaptedModelConfiguration: called by function calibrateModel()
fitnessAdaptedModelConfiguration <- function 
(
  parameterValue, parameterName, configuration, wellPattern 
  ) 
  {
  configuration <- wtSetParameter(configuration, parameterName, parameterValue)
  
  wtaqResult <- wtRunConfiguration(configuration)
  
  modelFitnessAggregated(wtaqResult, wellPattern)
  }

#calibrateModel()
calibrateModel <- function (
    ### WTAQ parameterisation, e.g. as retrieved by wtConfigure()
    configuration,
    ### regular expression or name of well(s) to be used for calibration: e.g. "OW4"
    wellPattern,
    ### name of ONE WTAQ parameter to be calibrated: e.g. `hkr`, `sw`
    parameterName,
    ### min/max range of possible calibration parameter values 
    parameterRange 
    ) 
    {
    optResults <- optimise(
      f = fitnessAdaptedModelConfiguration, 
      interval = parameterRange,
      parameterName = parameterName, 
      configuration = configuration, 
      wellPattern = wellPattern )
    
    ### Save calibrated WTAQ configuration: 
    wtaqConfigurationCalibrated <- wtSetParameter(
      configuration = configuration, 
      parameterName = parameterName, 
      parameterValue = optResults$minimum)
    
    ### Save optimisation results in list
    list(parameterName=parameterName, 
         wellPattern=wellPattern,
         optimalParameterValue=optResults$minimum, 
         minimalPerformanceValue=optResults$objective,
         wtaqConfig=wtaqConfigurationCalibrated
         )
    
    }

## ---- eval=TRUE----------------------------------------------------------
# 1.Step: Calibrate aquifer characteristics 'hkr' for OW4-------------------------------

calibratedAquifer <- calibrateModel(
  ### reference wtaq configuration
  configuration = wtaqConfiguration, 
  ### calibrate hydraulic aquifer conductivity
  parameterName = "hkr",  
  ### 'hkr' is within 0.0001 - 0.1 m/s
  parameterRange = c(0.0001, 0.1), 
  ### only use drawdown time-series of OW4 for calibration 
  wellPattern = "OW4" 
  )


### Plot the drawdowns with calibrated aquifer characteristics:
wtPlotResult(wtaqResult = wtRunConfiguration(
  configuration = calibratedAquifer$wtaqConfig),
  
  main = sprintf("Optimal value of parameter '%s' for %s: %0.4f m/s", 
                 calibratedAquifer$parameterName, 
                 calibratedAquifer$wellPattern, 
                 calibratedAquifer$optimalParameterValue),
  plottype = "w")

## ---- eval=TRUE----------------------------------------------------------
#2.Step: Calibrate well-bore skin parameter 'sw' for PW-------------------------------
calibratedAquiferAndWellSkin <- calibrateModel(
  #### WTAQ configuration with calibrated aquifer (Step1)
  configuration = calibratedAquifer$wtaqConfig,
  ### calibrate well-bore skin parameter
  parameterName = "sw", 
  ### 'sw' within 0 - 100 (dimensionless)
  parameterRange = c(0, 100), 
  ### only use drawdown time-series of PW for calibration 
  wellPattern = "PW"  
  )

### Plot the drawdowns with calibrated aquifer & well-bore skin characteristics:
wtPlotResult(wtaqResult = wtRunConfiguration(
  configuration = calibratedAquiferAndWellSkin$wtaqConfig,
  ),
  
  main = sprintf("Optimal value of parameter '%s' for %s: %2.4f (dimensionless)", 
                 calibratedAquiferAndWellSkin$parameterName, 
                 calibratedAquiferAndWellSkin$wellPattern, 
                 calibratedAquiferAndWellSkin$optimalParameterValue),
  plottype = "w")


## ---- echo=TRUE----------------------------------------------------------
### Well-skin parameter from calibration of W5 for all wells of the well field 
wellSkin <- calibratedAquiferAndWellSkin$optimalParameterValue
sprintf("Well-skin parameter value: %2.6f (dimensionless)", wellSkin)

# Example well field configuration with 5 wells
owWellfieldConf <- rbind(  
  
  owConfigureWell(
    wellName = "W1", x = 807679.64, y = 2091015.29, 
    r = 1.5, z1 = 2, z2 = 4, sw = wellSkin),  
  
  owConfigureWell(
    wellName = "W2", x = 807608.66, y = 2091018.51, 
    r = 1.7, z1 = 4.2, z2 = 7, sw = wellSkin),
  
  owConfigureWell(
    wellName = "W3", x = 807558.27, y = 2091090.30, 
    r = 1.5, z1 = 1.8, z2 = 8, sw = wellSkin),
  
  owConfigureWell(
    wellName = "W4", x = 807509.29, y =  2091161.80, 
    r = 1.5, z1 = 1.8, z2 = 7.5, sw = wellSkin),
  
  owConfigureWell(
    wellName = "W5", x = 807458.95, y =  2091232.26, 
    r = 1.5, z1 = 0.4, z2 = 7.8, sw = wellSkin))

## ---- echo=FALSE---------------------------------------------------------
distanceMatrix <- round(dist(owWellfieldConf[,c("x","y")],
                             method="euclidian", 
                             upper=TRUE,),
                        1)
attr(distanceMatrix, "Labels") <- owWellfieldConf$wellName
cat("Well distance matrix table:")
cat("")
distanceMatrix

## ---- echo=TRUE----------------------------------------------------------
# Create constant components of WTAQ configuration
owConf <- owConfigure(wellfield = owWellfieldConf,
                      aquifer = calibratedAquiferAndWellSkin$wtaqConfig$aquifer,
                      drainage = wtConfigureDrainage(idra = 0)
                     )

## ---- echo=TRUE----------------------------------------------------------
calibratedAquiferAndWellSkin$wtaqConfig$aquifer

## ---- echo=TRUE, fig.height=7, fig.width=10------------------------------
owPlotConfiguration(owConf,  referenceWell = 1)

## ---- echo=TRUE----------------------------------------------------------
# pumping rates of well W1 to W5
pumpingRates <- c(0.069, 0.089, 0.088, 0.087, 0.086) 

### user-specified times
timesForDDcalculation <- c(1,5,10,30,60,600,1200, 2400,3600, 
                           10000, 50000, 100000, 300000) 

## ---- echo=TRUE----------------------------------------------------------
ddlist <- owGetDrawdowns(
  owConf=owConf, 
  Q = pumpingRates, 
  times = timesForDDcalculation,
  ### required for using the function wtPlotResults()
  to.matrix=FALSE 
  )

ddmatrix<- owGetDrawdowns(
  owConf=owConf, 
  Q = pumpingRates, 
  times = timesForDDcalculation,
  ### saves results in "matrix" format: required for function owSuperposeDrawdowns()
  to.matrix=TRUE 
  )

## ----echo=TRUE-----------------------------------------------------------
drawdownsWithWellInterference <- owSuperposeDrawdowns(drawdownList = ddmatrix)

## ----echo=TRUE-----------------------------------------------------------
myWell <- "W1"
DD_inProductionWell_withInterference <- drawdownsWithWellInterference[,myWell] 
DD_inProductionWell_withoutInterference <- ddmatrix[[myWell]][,myWell]
diffDrawdown <- DD_inProductionWell_withInterference-DD_inProductionWell_withoutInterference
data.frame(time.in.seconds=drawdownsWithWellInterference[,1],
      additionalDrawdown.due.to.wellInterference.in.meter=diffDrawdown)

## ---- echo=TRUE----------------------------------------------------------
for (wellName in owWellfieldConf$wellName)
{
  index <- which(owWellfieldConf$wellNam==wellName)
 wellQ <- pumpingRates[index]
 if (wellQ > 0)
 {
  wtPlotResult(wtaqResult = ddlist[[wellName]], 
               plottype = "s", 
               ylab = "Drawdown (m)",
               xlab = "Time in seconds since start of pumping (s)",
               main = sprintf("Pumping well: %s (Q: %3.1f m3/h)", 
                              wellName,                                   
                              wellQ*3600))
 }
}

## ---- echo=TRUE----------------------------------------------------------
plotWellInterference <- function(wellName, Q=NULL)
{
  if (is.null(Q)) {
    label <- sprintf("%s ",wellName)
    } else {
      label <- sprintf("%s (%3.1f m3/h) ",wellName, Q*3600)
    }
  
  drawdown <- data.frame(SCENARIO="no well interference", 
                         TIME=ddmatrix[[wellName]][,"TIME"], 
                         CALCDD=ddmatrix[[wellName]][,wellName])
  drawdownWithInterference <- data.frame("SCENARIO"="with well interference", 
                         TIME=drawdownsWithWellInterference[,"TIME"], 
                         CALCDD=drawdownsWithWellInterference[,wellName])
  
  res <- rbind(drawdown, drawdownWithInterference)
  print(xyplot(CALCDD ~ TIME, groups =  SCENARIO, ylim=rev(extendrange(res$CALCDD)),
               ylab = "Drawdown (m)",
               xlab = "Time in seconds since start of pumping (s)",
         type="b", auto.key=list(columns=2), data=res, main=label))
}

for (wellName in owWellfieldConf$wellName)
{
  index <- which(owWellfieldConf$wellNam==wellName)
 wellQ <- pumpingRates[index]
 plotWellInterference(wellName = wellName, Q = wellQ)
}

