## ---- echo = FALSE-------------------------------------------------------
parameterFile <- system.file("extdata", "parameterlist.txt", package = "kwb.wtaq")
parameterInfo <- read.table(parameterFile, header = TRUE, sep="@", quote = "", 
                            fill = TRUE, stringsAsFactors=FALSE)
parameterInfo <- kwb.utils::hsRenameColumns(
  parameterInfo, 
  list(FunctionName = "Function", 
       ParameterName = "Parameter", 
       DefaultValue = "Default value"))

parameterInfo$Function <- kwb.utils::hsTrim(parameterInfo$Function)

printFunctionTable <- function(parameterInfo, functionName) {
  caption <- paste("Parameters with their default value and a description ",
                    "for the function `kwb.wtaq::", functionName, "`", sep = "")
  knitr::kable(
    parameterInfo[parameterInfo$Function == functionName, -1], 
    row.names = FALSE,
    caption = caption)
}

## ---- echo = FALSE, results = 'asis'-------------------------------------
printFunctionTable(parameterInfo, "wtConfigureGeneral")

## ---- echo = FALSE, results = 'asis'-------------------------------------
printFunctionTable(parameterInfo, "wtConfigureAquifer")

## ---- echo = FALSE, results = 'asis'-------------------------------------
printFunctionTable(parameterInfo, "wtConfigureDrainage")

## ---- echo = FALSE, results = 'asis'-------------------------------------
printFunctionTable(parameterInfo, "wtConfigureTimes")

## ---- echo = FALSE, results = 'asis'-------------------------------------
printFunctionTable(parameterInfo, "wtConfigureSolution")

## ---- echo = FALSE, results = 'asis'-------------------------------------
printFunctionTable(parameterInfo, "wtConfigurePumpwell")

## ---- echo = FALSE, results = 'asis'-------------------------------------
printFunctionTable(parameterInfo, "wtConfigureObservationWell")

