# wtSetParameters ------------------------------------------------------------
wtSetParameters <- function # set parameters in WTAQ configuration
### set numerical, scalar parameters in WTAQ configuration
(
  configuration, 
  ### WTAQ configuration as returned by \code{\link{wtConfigure}}
  assignments = NULL
  ### list of "name = value" pairs defining the parameter assignments, e.g.
  ### list(hkr = 0.001, hkz = 0.002) to set the horizontal hydraulic 
  ### conductivity (hkr) to 0.0001 (length/time) and the vertical hydraulic 
  ### conductivity (hkz) to 0.00005 (length/time). For the allowed parameter
  ### names see the description in \code{\link{wtSetParameter}}
)
{
  ##seealso<< \code{\link{wtSetParameter}}
  
  for (parameterName in names(assignments)) {
    configuration <- wtSetParameter(
      configuration,
      parameterName,
      assignments[[parameterName]])
  }
  
  configuration
  ### \emph{configuration} with adapted parameter values as defined in 
  ### \emph{assignments}
}

# wtSetParameter -------------------------------------------------------------
wtSetParameter <- function # set parameter in WTAQ configuration
### set numerical, scalar parameter in WTAQ configuration
(
  configuration, 
  ### WTAQ configuration as returned by \code{\link{wtConfigure}}  
  parameterName, 
  ### parameter name. Must be one of the Aquifer parameters "bb", "hkr", "hkz", 
  ### "ss", "sy" (see \code{\link{wtConfigureAquifer}} or one of the Drainage 
  ### parameters "acc", "akk", "amm", "axmm" (see
  ### \code{\link{wtConfigureDrainage}}) or one of the Pumpwell parameters
  ### "qq", "rw", "rc", "zpd", "zpl", "sw" (see \code{\link{wtConfigurePumpwell}})
  parameterValue
  ### numeric value to which the parameter \emph{parameterName} shall be set
)
{
  ##seealso<< \code{\link{wtSetParameters}}
  
  stopifnot(is.character(parameterName) && length(parameterName == 1))
  
  accepted.aquifer <- c("bb", "hkr", "hkz", "ss", "sy")
  accepted.drainage <- c("acc", "akk", "amm", "axmm")
  accepted.pumpwell <- c("qq", "rw", "rc", "zpd", "zpl", "sw")  
  
  if (parameterName %in% accepted.aquifer) {
    configuration$aquifer[[parameterName]] <- parameterValue
  }
  else if (parameterName %in% accepted.drainage) {
    configuration$drainage[[parameterName]] <- parameterValue
  }
  else if (parameterName %in% accepted.pumpwell) {
    configuration$pumpwell[[parameterName]] <- parameterValue
  }
  else {
    accepted.text <-paste(
      paste("Aquifer-parameters:", commaCollapsed(hsQuoteChr(accepted.aquifer))),
      paste("Drainage-parameters:", commaCollapsed(hsQuoteChr(accepted.drainage))),
      paste("Pumpwell-parameters:", commaCollapsed(hsQuoteChr(accepted.pumpwell))),
      sep = "\n")
    
    errorMessage <- sprintf(
      "The parameter name '%s' is not in the list of accepted parameters:\n%s",
      parameterName, accepted.text)
    
    stop(errorMessage)    
  }

  configuration
  ### \emph{configuration} with the parameter \emph{parameterName} altered to
  ### the value given in \emph{parameterValue}
}

# wtReadInputFile --------------------------------------------------------------
wtReadInputFile <- function # Read WTAQ configuration from input file
### Reads a WTAQ configuration (as e.g. required by
### \code{\link{wtRunConfiguration}}) from an existing WTAQ input file.
(
  inputFile,
  ### full path to an existing WTAQ input file
  dbg = FALSE
  ### if TRUE, debug message are shown, else not.  
)
{
  ##seealso<< \code{\link{wtRunInputFile}, \link{wtConfigure}}
  
  txt <- readLines(inputFile)
  if (length(grep("DIMENSIONAL", txt[2])) < 1) {
    stop('Only input files with format = "DIMENSIONAL" (in line 2) supported!')
  }
  gen <- wtConfigureGeneral(title=txt[1])
  
  # Line 3 
  if (length(grep("^\\s*WATER TABLE", txt[3])) > 0) {
    aqtype = "WATER TABLE"
  } else if (length(grep("^\\s*CONFINED", txt[3])) > 0) {
    aqtype = "CONFINED"
  } else {
    stop('Unknown aquifer type in line 3 (must be "WATERTABLE" or "CONFINED")')
  }
  
  # Line 4
  v <- as.numeric(sub("[dD]", "E", .lineSplitAtSpace(txt[4])[1:5]))
  aqu <- wtConfigureAquifer(aqtype=aqtype, bb=v[1], hkr=v[2], hkz=v[3], 
                       ss=v[4], sy=v[5])
  
  # Line 5
  v <- as.numeric(.lineSplitAtSpace(txt[5])[1:2])
  idra   <- v[1]
  nalpha <- v[2]
  
  # Line 6
  parts <- .lineSplitAtSpace(txt[6])

  # ignore parts starting with a letter
  v <- as.numeric(sub("[dD]", "E", parts[grep("^\\D", parts, invert=TRUE)]))
  v <- v[!is.na(v)]
  alpha <- NULL  
  if (length(v) < 1) {
    stop("Not at least one value found in line 6")
  }
  if (idra == 0) {
    if (v[1] != 1.0e9) {
      stop("Value 1.0D09 not found in line 6 (expected since IDRA = 0)")
    }
    drn <- wtConfigureDrainage(idra)
  } else if (idra == 1) {
    if (length(v) > 5) {
      stop("More than five values found in line 6 (max. 5 expected since IDRA = 1)")
    }
    drn <- wtConfigureDrainage(idra, alpha=v)
  } else if (idra == 2) {
    drn <- wtConfigureDrainage(idra, acc=v[1], akk=v[2], amm=v[3], axmm=v[4])
  } else {
    stop("Unexpected value (", idra, ") for IDRA in line 5 (should be 0, 1, or 2)")
  }
  
  # Line 7
  v <- as.numeric(.lineSplitAtSpace(txt[7])[1:2])
  its <- v[1]  
  imeas = v[2]
  
  # Line 8
  v <- as.numeric(sub("[dD]", "E", .lineSplitAtSpace(txt[8])[1:3]))
  tms <- wtConfigureTimes(its = its, tlast=v[1], nlc=v[2], nox=v[3])
  
  # Line 9
  isoln <- as.numeric(.lineSplitAtSpace(txt[9])[1])
  
  # Line 10
  v <- as.numeric(sub("[dD]", "E", .lineSplitAtSpace(txt[10])[1:5]))
  
  if(isoln == 1) {
    sol <- wtConfigureSolution(isoln=isoln, rerrnr=v[1], rerrsum=v[2], nmax=v[3], ntms=v[4], ns=v[5])
  } else if(isoln == 2) {
    sol <- wtConfigureSolution(isoln=isoln, rerrnr=v[1], error=v[2], ntms=v[3], nnn=v[4], method=v[5])
  } else {
    stop("Unexpected value (", isoln, ") for ISOLN in line 9 (should be 1 or 2)")
  }
  
  # Line 11
  v <- as.numeric(.lineSplitAtSpace(txt[11])[1:3])
  c1 <- list(ipws=v[1], ipwd=v[2], ipump=v[3])
  
  # Line 12
  v <- as.numeric(sub("[dD]", "E", .lineSplitAtSpace(txt[12])[1:6]))
  c2 <- list(qq=v[1], rw=v[2], rc=v[3], zpd=v[4], zpl=v[5], sw=v[6])
  
  # number of expected columns (time only or time and measured drawdown)
  nc <- 1 + imeas 
  
  # Line 13
  if (its != 0) {
    v <- as.numeric(.lineSplitAtSpace(txt[13])[1:2])
    ntspw <- v[1]
    irun  <- v[2]
    
    # Lines 13 + i, i = 1 .. ntspw
    tdat <- .wtReadInputFileTable(txt, nrows=ntspw, lastrow=13, nc=nc)
    pw <- wtConfigurePumpwell(irun=irun, ipws=c1$ipws, ipwd=c1$ipwd, ipump=c1$ipump, 
                         qq=c2$qq, rw=c2$rw, rc=c2$rc, zpd=c2$zpd, zpl=c2$zpl, 
                         sw=c2$sw, tspw=tdat)
    # current row
    crow <- 14 + ntspw 
  } else {
    pw <- wtConfigurePumpwell(ipws=c1$ipws, ipwd=c1$ipwd, ipump=c1$ipump, 
                         qq=c2$qq, rw=c2$rw, rc=c2$rc, zpd=c2$zpd, zpl=c2$zpl, 
                         sw=c2$sw, tspw=NULL)
    # current row
    crow <- 13
  }
  
  # observation wells
  nobwc <- as.numeric(.lineSplitAtSpace(txt[crow])[1])
  
  # prepare list of observation wells
  obws <- list()
  
  # loop through observation wells
  for (i in seq(1, by = 1, length.out=nobwc)) {
    
    kwb.utils::catIf(dbg, "observation well #", i, ", starting at row ", crow+1, "\n")
    
    strs <- .lineSplitAtSpace(txt[crow+1])
    v <- as.numeric(strs[2:3])
    c1 <- list(obname=strs[1], iows=v[1], idpr=v[2])
    v <- as.numeric(sub("[dD]", "E", .lineSplitAtSpace(txt[crow+2])[1:6]))
    c2 <- list(r=v[1], z1=v[2], z2=v[3], zp=v[4], rp=v[5], xll=v[6])
    if (its != 0) {
      v <- as.numeric(.lineSplitAtSpace(txt[crow+3])[1:2])
      ntsob <- v[1]
      irun  <- v[2]
      
      # read table data
      tdat <- .wtReadInputFileTable(txt, nrows=ntsob, lastrow=crow+3, nc=nc)
      
      # create obswell configuration
      obw <- wtConfigureObservationWell(
        irun=irun, obname=c1$obname, iows=c1$iows, idpr=c1$idpr, 
        r=c2$r, z1=c2$z1, z2=c2$z2, zp=c2$zp, rp=c2$rp, xll=c2$xll, 
        tsobs=tdat)
      crow <- crow + 3 + ntsob
    } else {
      obw <- wtConfigureObservationWell(
        obname=c1$obname, iows=c1$iows, idpr=c1$idpr, 
        r=c2$r, z1=c2$z1, z2=c2$z2, zp=c2$zp, rp=c2$rp, xll=c2$xll, 
        tsobs=NULL)
      crow <- crow + 2
    }
    
    # append obswell configuration to list of obswell configurations
    obws[[i]] <- obw
  }
  
  wtConfigure(general = gen,
         aquifer = aqu, 
         drainage = drn, 
         times = tms, 
         solution = sol,
         pumpwell = pw,
         obswells = obws)
  
  ### list with elements \emph{general}, \emph{aquifer}, \emph{drainage}, 
  ### \emph{times}, \emph{solution}, \emph{pumpwell}, \emph{obswell}, 
  ### representing a WTAQ model run configuration.
}

# wtInputFileLines -------------------------------------------------------------
wtInputFileLines <- function # text lines for WTAQ input file
### This function transforms a WTAQ configuration as generated with
### \code{\link{wtConfigure}} into a vecotor of text lines. These text lines,
### written to a file, can be used as input file to the WTAQ drawdown modelling
### software.
(
  configuration = wtConfigure(), 
  ### WTAQ configuration as generated by \code{\link{wtConfigure}}.
  sep = "\t\t",
  ### Separator to be placed between parameter values and parameter names.
  ### Default: two tab characters.  
  dbg = FALSE
) 
{
  ##seealso<< \code{\link{wtReadInputFile}}
  
  # check the configuration (program stops on error)
  wtCheckConfiguration(configuration, dbg = dbg)
  
  # use "E" for single precision ("E" in Fortran) and 
  #     "e" for double precision ("D" in Fortran)
  vformat <- list(title = "%-40s", format = "%-40s", aqtype = "%-40s",
                  bb    = "%9.3e", hkr = "%9.3e", hkz = "%9.3e", 
                  ss    = "%9.3e", sy  = "%9.3e",
                  acc   = "%9.3e", akk = "%9.3e", amm = "%9.3e", axmm = "%9.3e",
                  idra  = "%d", nalpha = "%d",
                  its   = "%d", imeas  = "%d",
                  tlast = "%9.3e", nlc   = "%d", nox    = "%d",
                  isoln = "%d", 
                  rerrnr = "%9.3e", rerrsum = "%9.3e", error = "%9.3e",
                  nmax  = "%d", ntms = "%d", ns     = "%d",
                  ntms  = "%d", nnn  = "%d", method = "%d",
                  ipws  = "%d", ipwd = "%d", ipump  = "%d",
                  qq    = "%9.2e", rw  = "%9.2e", rc  = "%9.2e", 
                  zpd   = "%9.2e", zpl = "%9.2e", sw  = "%9.2e",
                  r     = "%9.2e", z1  = "%9.2e", z2  = "%9.2e", 
                  zp    = "%9.2e", rp  = "%9.2e", xll = "%9.2e",
                  ntspw = "%d", irun = "%d",
                  nobwc = "%d",
                  iows  = "%d", idpr = "%d",
                  ntsob = "%d")
  
  p <- configuration # shortcut
  
  # Calculate dependent parameter values
  # - imeas = 1: measured drawdown data specified for each time
  # - nobwc: number of observation wells
  imeas <- as.integer(!is.null(p$pumpwell$tspw) && ncol(p$pumpwell$tspw) > 1)
  nobwc = length(p$obswells)
  lines <- c()  
  
  # Lines 1 and 2
  pv <- p$general
  pv$title <- substr(pv$title, 1, 70) # cut title to 70 characters
  lines <- c(lines, .wtInputFileLine(pv, "title", vformat, sep, dbg))
  lines <- c(lines, .wtInputFileLine(pv, "format", vformat, sep, dbg))
  
  # Lines 3 and 4
  pv <- p$aquifer
  lines <- c(lines, .wtInputFileLine(pv, "aqtype", vformat, sep, dbg))
  lines <- c(lines, .wtInputFileLine(pv, c("bb", "hkr", "hkz", "ss", "sy"), vformat, sep, dbg))
  
  # Lines 5 and 6
  pv <- p$drainage
  pv$nalpha <- length(pv$alpha)
  lines <- c(lines, .wtInputFileLine(pv, c("idra", "nalpha"), vformat, sep, dbg))

  if (pv$idra == 0) {
    lines <- c(lines, sprintf("1.0D09%s", sep))
  } else if (pv$idra == 1) {
    lines <- c(lines, sprintf("%s%sALPHA(I)",
                              paste(pv$alpha, collapse = " "), sep))
  } else {
    lines <- c(lines, .wtInputFileLine(pv, c("acc", "akk", "amm", "axmm"), vformat, sep, dbg))
  }
  
  # Lines 7 and 8
  pv <- p$times
  pv$imeas <- imeas
  lines <- c(lines, .wtInputFileLine(pv, c("its", "imeas"), vformat, sep, dbg))
  lines <- c(lines, .wtInputFileLine(pv, c("tlast", "nlc", "nox"), vformat, sep, dbg))
  
  # Lines 9 and 10
  pv <- p$solution
  lines <- c(lines, .wtInputFileLine(pv, c("isoln"), vformat, sep, dbg))
  if (pv$isoln == 1) {
    pnames <- c("rerrnr", "rerrsum", "nmax", "ntms", "ns")
  } else if (pv$isoln == 2) {
    pnames <- c("rerrnr", "error", "ntms", "nnn", "method")
  }
  lines <- c(lines, .wtInputFileLine(pv, pnames, vformat, sep, dbg))
  
  # Lines 11 and 12
  pv <- p$pumpwell
  lines <- c(lines, .wtInputFileLine(pv, c("ipws", "ipwd", "ipump"), vformat, sep, dbg))
  lines <- c(lines, .wtInputFileLine(pv, c("qq", "rw", "rc", "zpd", "zpl", "sw"), vformat, sep, dbg))
  
  # Lines 13 and 14 (+i)
  if (p$times$its != 0) {
    
    # Lines 13 + 14 (+i)
    lines <- c(lines, .wtInputFileTable(pv, pv$tspw, vformat, sep, dbg))
  }
  
  # Line 15
  lines <- c(lines, sprintf("%s%sNOBWC", nobwc, sep))
  
  for (i in seq(1, by = 1, length.out = nobwc)) {
    
    # Line 16
    pv <- configuration$obswells[[i]]
    lines <- c(lines, .wtInputFileLine(pv, c("obname", "iows", "idpr"), vformat, sep, dbg))
    
    # Line 17
    lines <- c(lines, .wtInputFileLine(pv, c("r", "z1", "z2", "zp", "rp", "xll"), vformat, sep, dbg))
    
    if (p$times$its != 0) {
      # Lines 18 + 19 (+i)
      lines <- c(lines, .wtInputFileTable(pv, tseries = pv$tsobs, vformat, sep, dbg))
    }
  }
  
  lines
  ### character vector with each element representing one row of the input file.
}

# wtRunConfiguration -----------------------------------------------------------
wtRunConfiguration <- function # Run WTAQ with given configuration
### Run a WTAQ simulation with the given configuration
(
  configuration,
  ### WTAQ configuration, as retrieved by \code{\link{wtConfigure}}.
  wtaq.exe = .wtaq_path(),
  ### full path to WTAQ executable (default: compiled executable in package
  ### subfolder "extdata" as defined in helper function .wtaq_path() )
  targetDirectory = tempdir(),
  ### optional. Target directory. If no target directory is given, a temporary
  ### directory will be used.
  show.results = FALSE,
  ### if TRUE, the content of the results file will be shown in the R console.
  ### Default: FALSE
  fileExtension = "",
  ### extension given to files
  dbg = FALSE,
  ### if TRUE, debug message are shown, else not. Default: FALSE
  ...
  ### further arguments passed to wtRunInputFile, e.g.
  ### \emph{show.output.on.console}
)
{
  ##seealso<< \code{\link{wtRunInputFile}}
  
  # "repair" configuration according to parameter dependencies
  configuration <- wtRepairConfiguration(configuration)
  
  # Create input file lines from parameter setting
  inpLines = wtInputFileLines(configuration, dbg = dbg)
  
  # Create path to input file  
  
  inputFile <- file.path(targetDirectory, "wtaqByR.inp")
  
  # Write input file
  write(inpLines, inputFile)
  
  # Get result of running the model
  wtaqResult <- wtRunInputFile(
    inputFile, 
    wtaq.exe, 
    targetDirectory, 
    show.results, 
    copyToTarget = FALSE, 
    configuration = configuration, 
    dbg = dbg,
    ...)
  
  # Return the model result
  wtaqResult
  
  ### model result as read with \code{\link{wtReadPlotFile}} from the output
  ### file generated by the WTAQ modelling software
}

# wtRunInputFile ---------------------------------------------------------------
wtRunInputFile <- function # Run WTAQ with given input file
### Run a WTAQ simulation with the given input file
(
  inputFile,
  ### Existing WTAQ input file
  wtaq.exe = .wtaq_path(),
  ### full path to WTAQ executable (default: compiled executable in package
  ### subfolder "extdata" as defined in helper function .wtaq_path() )
  targetDirectory = tempdir(),
  ### optional. Target directory. If no target directory is given, a temporary
  ### directory will be used.
  show.results = FALSE,
  ### if TRUE, the content of the results file will be shown in the R console.
  ### Default: FALSE
  copyToTarget = TRUE,
  ### if TRUE, the input file is copied to the target directory. Set this
  ### argument to FALSE if the input file already is in the target directory
  ### and does not need to be copied again.
  configuration = NULL,
  ### WTAQ configuration object as retrieved by \code{\link{wtConfigure}}. If
  ### not given (default), it will be constructed from the input file
  batchRun = FALSE, 
  ### if TRUE batch run (may require admin rights!), else using direct command, default: FALSE
  dbg = FALSE
  ### if TRUE, debug messages are shown, else not. Default: FALSE
)
{
  ##seealso<< \code{\link{wtInputFileLines}, \link{wtReadInputFile},
  ##\link{wtRunConfiguration}}
  
  
  if (! file.exists(inputFile)) {
    stop("WTAQ input file does not exist: ", inputFile)
  }

  if (! file.exists(wtaq.exe)) {
    stop("Could not find path to WTAQ executable: ", wtaq.exe)
  }
  
  ### Change permission on unix so that wtaq.exe is executable
  .chmod_wtaq(wtaq.exe)

  # Copy the file to the target directory unless this option is suppressed
  if (copyToTarget) {
    kwb.utils::catIf(dbg, sprintf("*** Copying %s to %s... ", inputFile, targetDirectory))
    file.copy(inputFile, targetDirectory)
    kwb.utils::catIf(dbg, "ok.\n")
  }  

  # Get filename of input file without extension
  parts <- strsplit(basename(inputFile), "\\.")[[1]]  
  file.base <- paste(parts[-length(parts)], collapse=".")
  

  # Arguments file
  argumentsFile <- file.path(targetDirectory, sprintf("%sArgs.txt", file.base))
  
  # File names: 1. <file.base>.inp, 2. <file.base>.out, 3. <file.base>.plot
  fileNames <- c(basename(inputFile), sprintf("%s.%s", file.base, c("out", "plot")))
  
  # cmd command line string for calling WTAQ with files read from arguments file
  #Windows command
  platform <- .Platform$OS.type

  if (platform == "windows") {
    cmd <- sprintf("cmd /C \"%s\" < \"%s\"", wtaq.exe, argumentsFile)
  }
  else if (platform ==  "unix") {
    cmd <- sprintf("'%s' < '%s'", wtaq.exe, argumentsFile)
  }
  else {
    stop("Platform '", platform, "' not supported (only windows, unix)!")
  }
  
  # Save current working directory, set working directory to targetDirectory
  # and reset working directory on exit
  cdir <- getwd()
  setwd(targetDirectory)  
  on.exit(setwd(cdir))
  
  # Rewrite arguments file
  write(fileNames, argumentsFile)
  

  kwb.utils::catIf(dbg, "\n*** Running WTAQ in", targetDirectory, "...\n")
  
  # Run WTAQ
  if(batchRun == FALSE)
  {
   kwb.utils::catIf(dbg, sprintf("WTAQ command run (OS: %s):\n", platform))
   if (platform ==  "windows") shell(cmd) ### not supported under Linux
   if (platform ==  "unix") system(cmd)
  } else {
    # Create batch file
    if (platform ==  "windows") 
    {
      kwb.utils::catIf(dbg, sprintf("WTAQ command run (OS: %s, WTAQ path: %s):\n", platform, wtaq.exe))
      batchFile <- file.path(targetDirectory, sprintf("%sRun.bat", file.base))
      write(paste("@ECHO OFF", cmd, "pause", sep = "\n"), batchFile)
    }
    if (platform ==  "unix") 
    {
      batchFile <- file.path(targetDirectory, sprintf("%sRun.sh", file.base))
      write(paste("#!/bin/bash", "# init", cmd , " ", sep = "\n"), batchFile)
    }
    kwb.utils::catIf(dbg, sprintf("WTAQ batch run (OS: %s, path: %s):\n", platform, wtaq.exe))
    system(batchFile)
  }
  kwb.utils::catIf(dbg, sprintf("Write results in target folder: %s\n", targetDirectory))
  
   # Read output file
  out <- readLines(file.path(targetDirectory, fileNames[2]))
  
  # Did WTAQ fail?
  fail <- length(grep("PROGRAM STOPPED.", out) > 0)
  
  # Show content of output file in console
  if (show.results || fail) {
    cat(paste(out, collapse = "\n"), "\n\n")
  }
  
  # if WTAQ failed, open input file and stop  
  if (fail) {
    # open input file in editor
    file.show(inputFile)
    stop("WTAQ failed. Check the auto-generated input file (opened in editor).")
  }
  
  # Get WTAQ configuration if not given
  if (is.null(configuration)) {
    configuration <- wtReadInputFile(inputFile)
  }
  
  # Logarithmic times?
  logtimes <- (configuration$times$its == 0)  
  
  # Get content of plot file
  wtaqResult <- wtReadPlotFile(fileNames[3], logtimes=logtimes, toListView = TRUE)
  
  # Rename wells according to the names in the configuration
  wellNames <- wtConfiguredWellnames(configuration)

  # Give observation well name as defined in configuration
  for (i in 1:length(wellNames)) {
    
    if (i == 1) {
      wellNameInWtaqResult <- "PW"
    } 
    else {
      wellNameInWtaqResult <- sprintf("OB%d", i-1)
    }
    
    wtaqResult$WELL[wtaqResult$WELL == wellNameInWtaqResult] <- wellNames[i]
  }    
  
  # Set WTAQ configuration as attribute "wtaqConfiguration"
  attr(wtaqResult, "wtaqConfiguration") <- configuration
  
  # Return content of plot file
  wtaqResult
  
  ### model result as read with \code{\link{wtReadPlotFile}} from the output
  ### file generated by the WTAQ modelling software  
}

# .wtRunScenarios --------------------------------------------------------------
.wtRunScenarios <- structure(
function # Run different scenarios based on input template
### Run different scenarios based on input template
(
  templateFile, 
  ### Full path to input template file
  parameterValues, 
  ### parameter values to be used for different WTAQ runs. List with each list
  ### element being a list of named parameter values 
  ### (\emph{list(parname1 = val1, parname2 = val2, ...)}) and being named 
  ### (scenario name).  
  parameterFormats, 
  ### named list of characters representing the format strings to be used in
  ### \emph{sprintf} function to generate formatted parameter values
  targetDirectory, 
  ### target directory in which input files are created and output files 
  ### produced by WTAQ will be written.
  wtaq.exe = .wtaq_path(), 
  ### full path to WTAQ executable file
  scenarioNames = names(parameterValues),
  ### character vector representing the names of scenarios to be evaluated.
  ### Default: names of list elements in \emph{parameterValues}
  markerDelimiter = "%" 
  ### marker delimiter character. Default: percent sign
) 
{ 
  .chmod_wtaq(wtaq.exe) 
  
  
  # Read input file template
  tpl.lines <- readLines(templateFile)
  
  # Loop through scenarios
  for (scn in scenarioNames) {
    
    cat("\n*** Scenario:", scn)
    
    # Set parameters in template
    inp.lines <- .wtSetParameters(tpl.lines, parameterValues[[scn]], parameterFormats, markerDelimiter)
    
    # Generate input file name
    inp.file <- file.path(targetDirectory, sprintf("%s_hs.%s", scn, "inp"), fsep = "\\")
    
    # Write input file
    write(inp.lines, inp.file)
    
    # Run WTAQ with given input file
    wtRunInputFile(inp.file)
  }
}, ex = function() 
{
  ### Get path to temporary directory
  targetDirectory <- tempdir()
  cat("\n*** Files are written to", targetDirectory, "\n")
  
   ### Change permission on unix so that wtaq.exe is executable
  .chmod_wtaq(wtaq.exe)   

  ### Define scenarios with different parameter settings
  parameterValues = list(s03 = list(bb=10, hkr=1.0E-03, hkz=1.0E-02, ss=1.E-05, sy=0.1),
               s04 = list(bb=10, hkr=1.0E-03, hkz=1.0E-05, ss=1.E-05, sy=0.1),
               s05 = list(bb=10, hkr=1.0E-03, hkz=1.0E-07, ss=1.E-05, sy=0.1),
               s06 = list(bb=10, hkr=1.0E-02, hkz=1.0E-02, ss=1.E-05, sy=0.1),
               s07 = list(bb=10, hkr=5.0E-03, hkz=5.0E-03, ss=1.E-05, sy=0.1),
               s08 = list(bb=10, hkr=5.0E-04, hkz=5.0E-04, ss=1.E-05, sy=0.1),
               s11 = list(bb=10, hkr=1.0E-03, hkz=1.0E-03, ss=1.E-05, sy=5.0E-02),                                                                           
               s12 = list(bb=10, hkr=1.0E-03, hkz=1.0E-03, ss=1.E-05, sy=3.5E-01),                                                                           
               s13 = list(bb=10, hkr=1.0E-03, hkz=1.0E-03, ss=1.E-03, sy=1.0E-01),
               s14 = list(bb=10, hkr=1.0E-03, hkz=1.0E-03, ss=1.E-07, sy=1.0E-01))                                                                          
  
  ### Define formatting string for parameters 
  parameterFormats = list(bb = "%f", hkr = "%0.1E", hkz = "%0.1E", ss = "%0.1E", 
                 sy = "%f")  
  
  # Run scenarios s03 to s08
  templateFile <- system.file("extdata", "s03_08_tpl.inp", package = "kwb.wtaq")
  .wtRunScenarios(
    templateFile, parameterValues, parameterFormats, targetDirectory, wtaq.exe, 
    scenarioNames = sprintf("s%02d", 3:8))  
  
  # Run scenarios s11 to s14
  .wtRunScenarios(
    templateFile, parameterValues, parameterFormats, targetDirectory, wtaq.exe, 
    scenarioNames = sprintf("s%02d", 11:14))
  
  # Read results plot files into one data frame
  alldat <- NULL
  for (pfile in dir(targetDirectory, "^s.+plot$", full.names = TRUE)) {
    tmpdat <- wtReadPlotFile(pfile)
    tmpdat$src <- basename(pfile)
    alldat <- rbind(alldat, tmpdat)
  }
  
  # Plot results
  print(xyplot(PW + OB1 + OB2 + OB3 ~ TIME | src, data = alldat, 
               type = "b", auto.key = list(columns = 4)))
})

# wtReadPlotFile ---------------------------------------------------------------
wtReadPlotFile <- structure(
function # Read WTAQ plot file
### Read WTAQ plot file
(
  plotfile, 
  ### plot file as produced by WTAQ model, either in \dQuote{list view}, i.e. 
  ### in which data is organised in blocks being written one after the other
  ###  or in \dQuote{matrix view}, i.e. in which data is organised in
  ### one table only
  logtimes = NULL,
  ### if TRUE, time steps are supposed to be logarithmic, i.e. it is assumed 
  ### that the result plot file is in \dQuote{matrix form}. Otherwise 
  ### (logtimes == FALSE) it is assumed that the result plot file is in 
  ### \dQuote{list form} as each data block can contain different timestamps.  
  ### Setting this argument accelerates the reading of the results as the result 
  ### plot file does not have to be read twice (once for the format, once for 
  ### the actual data).  
  toListView = FALSE,
  ### if TRUE, results are always returned in \dQuote{list view} in which 
  ### drawdowns of different wells do not appear in different columns but all in
  ### the same column \emph{CALCDD}. A column \emph{WELL} is added to indicate
  ### the well to which the drawdown belongs. Default: FALSE.
  dbg = FALSE
  ### if TRUE, debug messages are shown. Default: FALSE
)
{
  if (is.null(logtimes)) {
    logtimes <- !.wtIsInListView(plotfile)
    cat("Timesteps in plot file are expected to be:", 
        ifelse(logtimes, "logarithmic.\n", "user-specified.\n"))
  }
  
  if (logtimes) {
    res <- .wtReadPlotFileMatrixView(plotfile, dbg)

    # Convert original column names to well names
    names(res) <- sub("^HD(PW|OB)", "\\1", names(res))
    
    attr(res, "format") <- "wide"
    
    if (toListView) {
      cnames <- names(res)      
      
      # Columns 1 to (now renamed to) "PW" are key columns
      attribs <- attributes(res) # save attributes

      res <- hsMatrixToListForm(
        res, keyFields = cnames[1:(which(cnames == "PW") - 1)], 
        colNamePar = "WELL", colNameVal = "CALCDD")

      # Restore additional attributes
      res <- hsRestoreAttributes(res, attribs)
      attr(res, "format") <- "long"
    }            
  }
  else {
    res <- .wtReadPlotFileListView(plotfile, dbg)
    attr(res, "format") <- "long"        
  }

  # Return result data frame
  res
}, 
ex = function() 
{
  ### Read plot files as provided with "sample problems" 1 to 3 in WTAQ
  ### installation files
  pfile <- system.file("extdata", sprintf("plt.sp%d", 1:3), 
                       package = "kwb.wtaq")
  
  dat1 <- wtReadPlotFile(pfile[1])  
  dat2 <- wtReadPlotFile(pfile[2])
  dat3 <- wtReadPlotFile(pfile[3])
  
  ### Plot HDPW + HDOB1 + HDOB2 + HDOB3 + HDOB4 over dimensionless time (TDRDSQ)
  tr1 <- xyplot(PW + OB1 + OB2 + OB3 + OB4 ~ TDRDSQ, 
                data = dat1, type = "l", auto.key = list(columns = 5), 
                main = "Example 1")    
  
  ### Plot CALCDD over TIME
  tr2 <- xyplot(CALCDD ~ TIME, groups = WELL, 
                data = dat2, type = "l", auto.key = list(columns = 5),
                main = "Example 2")
  
  tr3 <- xyplot(CALCDD ~ TIME, groups = WELL, 
                data = dat3, type = "l", auto.key = list(columns = 5),
                main = "Example 3")
  print(tr1)
  print(tr2)
  print(tr3)  
})
