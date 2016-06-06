# checkModelFit ----------------------------------------------------------------
checkModelFit <- function # Calibration of aquifer parameter "hkr"
### Calibration of aquifer parameter "hkr"
(
  wtaqResult, 
  #### in WTAQ result in "long format"
  wells = ".*" 
  ### regular expression of wells to be included (e.g. ".*" for all, PW: only
  ### production well, OW: all observation wells)
)
{
  subresult <- wtaqResult[grep(pattern = wells, wtaqResult$WELL), ]
  
  fitting <- t(gof(sim = subresult$MEASDD, 
                   obs = subresult$CALCDD,
                   digits = 3))
  
  colnames(fitting) <- sub(" %", "", colnames(fitting))
  
  as.data.frame(fitting)
}

# wtCalibrate ------------------------------------------------------------------
wtCalibrate <- function # Simple calibration procedure
### Simple calibration procedure
(
  configuration,
  parameterName = "hkr",
  startvalue = 2E-2,
  endvalue = 1E-5,
  changevalue = 1E-4,
  wells = "OW4",
  fitnessCriteria = data.frame(
    ### stat. parameters for for model fit evaluation
    name = c("RMSE", "PBIAS", "PBIAS", "R2"), 
    ### min/max values for fitting criteria
    vals = c(0.03, 5, -5, 0.95),
    ###
    condition = c("<=", "<=", ">=",">=")
  )
)
{
  if (startvalue > endvalue & changevalue > 0) {
    changevalue <- -changevalue
  }
  
  calibrationStatistics <- data.frame()  
  
  parameterValues <- seq(from = startvalue, to = endvalue, by = changevalue)
  
  for (parameterValue in parameterValues)
  {
    aquiferParameters <- c("hkr", "hkz", "bb", "ss", "sy")
    wellParameters <- c("sw")
    
    if (parameterName %in% aquiferParameters) {
      configuration$aquifer[[parameterName]] <- parameterValue
    }
    else if (parameterName %in% wellParameters) {
      configuration$pumpwell[[parameterName]] <- parameterValue
    }
    else {
      stop(sprintf(
        "Selected parameter '%s' not available for calibration. Please
                 select one of the following Aquifer or Well parameters:\n
                 Aquifer parameters: %s\n
                 Well parameters: %s", parameterName,  
        paste(aquiferParameters, sep =" ", collapse = " "), 
        paste(wellParameters, sep =" ", collapse = " ")))
    }
    
    wtaqResult <- wtRunConfiguration(configuration, show.results = FALSE)
    
    fitting <- checkModelFit(wtaqResult = wtaqResult, wells = wells)
    
    currentResult <- data.frame(par = parameterValue, fitting = fitting)
    
    currentResult <- hsRenameColumns(currentResult, list(par = parameterName))
    
    calibrationStatistics <- rbind(calibrationStatistics, currentResult)
    
    expressionParts <- sprintf(
      "currentRes$%s %s %3.3f", 
      fitnessCriteria$name, fitnessCriteria$condition, fitnessCriteria$vals)

    expressionString <- paste(expressionParts, sep = " ", collapse = " && ")
    
    myFitCrits <- eval(parse(text = expressionString))
    
    if (myFitCrits)
    {
      #sprintf("Calibration stopped with the following result:\n: %2.7f ::: RMSE: %2.2f m,  PBIAS: %3.1f %%, R2: %1.2f", 
      #        myCaliConf$aquifer$hkr, currentRes$RMSE,currentRes$PBIAS, currentRes$R2  )
      list(
        fittedVal = parameterValue,
        fittedStats = currentResult,
        allStats = calibrationStatistics)
      
      break;
    }
  }
}

if (FALSE)
{
  #Validation of WTAQ aquifer parameterisation 
  myConf$aquifer$hkr <- caliHkr 
  myRes <- wtRunConfiguration(myCaliConf, show.results = FALSE)
  checkModelFit(myRes, wells = "OW4")[,c("RMSE", "PBIAS", "R2")]
  checkModelFit(myRes, wells = "OW3")[,c("RMSE", "PBIAS", "R2")]
  checkModelFit(myRes, wells = "OW2")[,c("RMSE", "PBIAS", "R2")]
  checkModelFit(myRes, wells = "OW1")[,c("RMSE", "PBIAS", "R2")]
  
  ### Calibration of well-skin parameter "sw"
  myCaliConf <- myConf
  caliStats <- data.frame()
  critRMSE <- 0.03
  critPBIAS <- 5
  critR2 <- 0.90
  caliSw <- NA
  for (mySw in seq(from=0, to=10, by=0.01))
  {
    myCaliConf$pumpwell$sw <- mySw
    myCaliRes <- wtRunConfiguration(myCaliConf,show.results = FALSE)
    currentRes <- data.frame(sw=mySw, checkModelFit(wtaqResult = myCaliRes, wells = "PW"))
    caliStats <- rbind(caliStats, currentRes)
    if (currentRes$RMSE <= critRMSE & 
          (currentRes$PBIAS >= -abs(critPBIAS) | currentRes$PBIAS <= abs(critPBIAS)) & 
          currentRes$R2 >= critR2)
    {
      sprintf("Calibration of well-skin parameter stopped with the following result:\nsw: %2.2f ::: RMSE: %2.2f m,  PBIAS: %3.1f %%, R2: %1.2f", 
              myCaliConf$pumpwell$sw, currentRes$RMSE,currentRes$PBIAS, currentRes$R2  )
      caliSw <- mySw
      break;}
  }
  myConf$pumpwell$sw <-  caliSw
  
  myRes <- wtRunConfiguration(myConf)
  wtPlotResult(myRes, plottype = c("w"))  
}

# modelFitness -----------------------------------------------------------------
modelFitness <- function # modelFitness
### modelFitness
(
  wtaqResult, 
  #### in "long format"
  wells = "*" 
  ### regular expression of wells to be included (e.g. * for all, PW: only
  ### production well, OW: all observation wells)
)
{
  subResult <- wtaqResult[grep(pattern = wells, wtaqResult$WELL),]
  
  fitness <- t(gof(sim = subResult$MEASDD, obs = subResult$CALCDD, digits = 3))
  
  colnames(fitness) <- sub(" %", "", colnames(fitness))
  
  as.data.frame(fitness)
}

# modelFitnessAggregated -------------------------------------------------------
modelFitnessAggregated <- function # modelFitnessAggregated
### modelFitnessAggregated
(
  wtaqResult, wellPattern = "*"
)
{
  fitness <- modelFitness(wtaqResult, wellPattern)
  1 - fitness$NSE
}

# fitnessAquifer ---------------------------------------------------------------
fitnessAquifer <- function # fitnessAquifer
### fitnessAquifer
(
  parameterValue, parameterName, configuration, wellPattern = "*") 
{
  configuration$aquifer[[parameterName]] <- parameterValue
  
  wtaqResult <- wtRunConfiguration(configuration, show.results = FALSE, dbg = FALSE)
  
  modelFitnessAggregated(wtaqResult, wellPattern)
}

# Testcalibration --------------------------------------------------------------
if (FALSE)
{
  #wtSetParameters(configuration, list(hkr = 0.011, hkz = 0.112))$aquifer
  
  parameterName <- "hkr"
  optimum <- list()
  
  for (well in c("PW", "OW1", "OW2", "OW3", "OW4")) {
    
    optimum[[well]] <- optimise(
      f = fitnessAquifer, 
      interval = c(0, 1), 
      parameterName = parameterName,
      configuration = wtaqConfiguration,
      wellPattern = well)
    
    wtPlotResult(
      data = wtRunConfiguration(
        wtSetParameter(wtaqConfiguration, parameterName, 
                       optimum[[well]]$minimum),
        dbg = FALSE), 
      
      main = sprintf("Optimal value of parameter '%s' for %s: %0.4f", 
                     parameterName, well, optimum[[well]]$minimum),
      plottype = "w")
  }
  
  optimum
}

