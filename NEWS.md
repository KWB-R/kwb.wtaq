# [kwb.wtaq 0.2.1](https://github.com/KWB-R/kwb.wtaq/releases/tag/v0.2.1) <small>2016-09-06</small>

**First Github release:**

* added /src files for WTAQ-2 from 
* added test for WTAQ2 calculation results on Windows

# kwb.wtaq 0.2.0 <small> Unreleased</small>

Developed within project 
[OPTIWELLS-2](https://www.kompetenz-wasser.de/en/project/optiwells-2-betriebsoptimierung-von-trinkwasserbrunnen/) 
(internal SVN repo rev1268)

# kwb.wtaq 0.1.3 <small> Unreleased</small>

### UCODE related
* Bug fix in ucPlotPng

# kwb.wtaq 0.1.2 <small> Unreleased</small>

### WTAQ related

* new: print.wtaqConf, wtPlotResult, 
* removed: wtPlotPWell, wtPlotOWell,
* renamed: wtPlotProfile -> wtPlotConf
* wtReadPlotLst: stop if file is empty, returns object of class "wtaqResult"
* wtReadPlotMtx: returns object of class "wtaqResult"
* wtReadInpFile: bug fix
* wtConf: returns object of class "wtaqConf"
* wtInpFileLines: new arg "dbg", float formats 9.3f/9.2f, 
* wtConfEx2, wtConfEx3: return object of class "wtaqConf", examples

### UCODE related

* new: ucPlot, ucReadSummary, ucPlotSummary
* ucPlotPng: arg "data" renamed -> "wtaqResult"


# kwb.wtaq 0.1.1 <small> Unreleased</small>

### WTAQ related
* new functions: wtReadInpFile, wtCheckConf
* wtConfDrainage, wtConfTimes, wtConfSolution, wtConfPumpwell: 
  numeric defaults
* wtInpFileLines: arg "pvals" renamed "conf", format consideration
* wtConf: argument check

### UCODE related

* support of "serial" and "parallel" running of UCODE/WTAQ
* names of UCODE-files modified
* ucRunParallel: generation of HTML- and png-files for calibration progress visualisation
* new functions: ucPlot, ucPlotPng, htmlMain, htmlCurrent, htmlHistory
* change in arguments: ucPrepCalib, ucWriteBatchUcode, ucWriteMainInput, 
  ucConf_Model_Command_Lines, ucInpFileLines

# kwb.wtaq 0.1.0 <small> Unreleased</small>

* depends on package hydroGOF

* Functions have been renamed: 
  wtGeneral       -> wtConfGeneral, 
  wtAquifer       -> wtConfAquifer,
  wtDrainage      -> wtConfDrainage, 
  wtTimes         -> wtConfTimes, 
  wtSolution      -> wtConfSolution, 
  wtPumpwell      -> wtConfPumpwell, 
  wtObswell       -> wtConfObswell, 
  wtParSetting    -> wtConf, 
  wtParSettingEx3 -> wtConfEx3
* New functions: 
  wtReadPlot, wtConfWellnames, wtDefaultConf
  functions related to UCODE, starting with "uc"
* Changes:
  wtRun: default for wtaq

# kwb.wtaq 0.0.1 <small> Unreleased</small>

**First creation of the package containing:**

* wtReadPlotLst()
* wtReadPlotMtx()
* wtSetPars()
* wtRunScenarios()
