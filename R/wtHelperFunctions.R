
.chmod_wtaq <- function(wtaq_exe_path) {
  if (.Platform$OS.type == "unix") {
    if (file.access(wtaq_exe_path , mode = 1) == -1) {
      Sys.chmod(wtaq_exe_path, mode = "0777", use_umask = TRUE)
    }
  }
}

#.wtaq_path (use for binary files compile from /src)---------------------------
#cause compiler warnings because machine constants are hard-coded in FORTRAN code
.wtaq_path <- function() {

  wtaq_name <- ""

  if (.Platform$OS.type ==  "windows") {
    wtaq_name <- "wtaq2_1.exe"
  } else {
    wtaq_name <- "wtaq2_1"
  }


  if (!(.Platform$OS.type == "unix" | .Platform$OS.type ==  "windows")) {
    stop(sprintf("Error, no WTAQ executable for platform %s available",
                      .Platform$OS.type))}

  system.file("bin", .Platform$r_arch, wtaq_name, package = "kwb.wtaq")

}

# # .wtaq_path (used for binary files)--------------------------------------------
# .wtaq_path <- function() {
#   
#   wtaq_name <- ""
#   
#   if (.Platform$OS.type ==  "windows") {
#     wtaq_name <- "wtaq.2.1.exe"
#   } 
#   else if (.Platform$OS.type == "unix") {
#     
#     if (R.version$arch == "i386") {
#       wtaq_name <- "wtaq.2.1_linux32bit.exe"} 
#     else if (R.version$arch == "x86_64") {
#       wtaq_name <- "wtaq.2.1_linux64bit.exe"} 
#     else { stop(sprintf("Error, no WTAQ executable for %s %s available", 
#                         .Platform$OS.type, 
#                         R.version$arch))}}
#   else { stop(sprintf("Error, no WTAQ executable for platform %s vailable", 
#                       .Platform$OS.type))}
#   
#   system.file("extdata", wtaq_name, package = "kwb.wtaq")
#   
# }




# .lineSplitAtSpace ------------------------------------------------------------
.lineSplitAtSpace <- function # split text line at spaces
(
  txtline
) 
{
  strsplit(sub("^\\s*", "", txtline), "\\s+")[[1]]
}

# .wtReadInputFileTable --------------------------------------------------------
.wtReadInputFileTable <- function
(
  txt, 
  nrows, 
  lastrow, 
  nc=1
) 
{
  tdat <- NULL
  for (i in seq(1, by = 1, length.out=nrows)) {
    v <- as.numeric(sub("[dD]", "E", .lineSplitAtSpace(txt[lastrow+i])[1:nc]))
    dat <- data.frame(t = v[1])
    if (nc > 1) {
      dat <- cbind(dat, data.frame(dd = v[2]))
    }
    tdat <- rbind(tdat, dat)
  }
  tdat
}

# .wtInputFileLine ---------------------------------------------------------------
.wtInputFileLine <- function
(
  pv, 
  paras, 
  vformat = NULL,
  sep = "\t\t",
  ### Separator to be placed between parameter values and parameter names.
  ### Default: two tab characters.    
  dbg = FALSE
) 
{
  formats <- list("character" = "%s", "numeric" = "%f")
  parts <- c()
  for (para in paras) {
    myval <- pv[[para]]
    mmode <- mode(myval)
    fmt <- vformat[[para]]
    if (is.null(fmt)) {
      fmt <- formats[[mmode]]
    } else {
      if (mmode == "character" && length(grep("s", fmt)) == 0) {
        fmt <- "%s"
      }
    }

    kwb.utils::catIf(dbg, "para: ", para, ", format: ", fmt, ", value: ", myval, "\n")

    part <- sprintf(fmt, myval)
    # if format contains "E", replace "E" with "D" (for use in Fortran)
    if (length(grep("e", fmt) > 0)) {
      part <- sub("e", "D", part)
    }
    # remove "+" in exponential number 
    if (length(grep("[eE]", fmt) > 0)) {
      part <- sub("\\+", "", part)
    }
    parts <- c(parts, part)
  }
  vnames <- ""
  if (paras[1] != "title") {
    vnames <- paste(toupper(paras), collapse = " ")
  }
  sprintf("%s%s%s", paste(parts, collapse = " "), sep, vnames)    
}

# .wtInputFileTable ------------------------------------------------------------
.wtInputFileTable <- function(pv, tseries, vformat, sep, dbg) {
  if ("tspw" %in% names(pv)) {
    # ntspw: number of user-specified times for which drawdown is calculated
    nname <- "ntspw"
    caption <- "%s%sTIMEPW(I) XMEASPW(I)"
  }
  else {
    nname <- "ntsob"
    caption <- "%s%sTIMEOB(I) XMEASOB(I)"
  }
  n <- ifelse(is.null(tseries), 0, nrow(tseries))  
  pv[[nname]] <- n
  lines <- .wtInputFileLine(pv, c(nname, "irun"), vformat, sep, dbg)
  
  for (i in seq(1, by = 1, length.out = n)) {
    line <- paste(tseries[i, ], collapse = " ")
    if (i == 1) {
      line <- sprintf(caption, line, sep)        
    }
    lines <- c(lines, line)
  }  
  lines
}

# .wtSetParameters -------------------------------------------------------------
.wtSetParameters <- function # Set parameters in template file lines
### Replaces placeholders of type \emph{<md>}\emph{parName}\emph{<md>} 
### (md = marker delimiter character, parName = parameter name as occurring 
### as list element name in \emph{pvals}) in input lines of template file
(
  lines, 
  ### character vector representing lines of input template file
  pvals,
  ### parameter values as named list elements with the names of the list 
  ### elements representing the parameter names
  pformat,
  ### named list of characters representing the format strings to be used in
  ### \emph{sprintf} function to generate formatted parameter valus
  md = "%" 
  ### marker delimiter character. Default: percent sign
) 
{  
  # Replace placeholders
  for (pname in names(pvals)) {
    ptrn <- sprintf("%s%s%s", md, pname, md)
    lines <- sub(ptrn,  sprintf(pformat[[pname]], pvals[[pname]]), lines, 
                 ignore.case = TRUE)    
  }
  
  # Return lines
  lines
  ### character vector representing lines of input template file in which
  ### placeholders \emph{<md>}\emph{parName}\emph{<md>} (md = marker delimiter 
  ### character, parName = parameter name as occurring as list element name in 
  ### \emph{pvals}) are replaced with corresponding parameter values (list 
  ### values in \emph{pvals}) given to this function.
}

# .wtIsInListView --------------------------------------------------------------
.wtIsInListView <- function
### Returns TRUE if WTAQ result file (.plot) is in \dQuote{list view} thus
### representing a WTAQ configuration where user-specified time steps have
### been calculated. Otherwise logarithmic times have been calculated and the
### WTAQ result file is in \dQuote{matrix view}. The distinction (for list view)
### is made by looking for "****   PUMPED WELL" in the file. 
(
  plotfile
) 
{
  ptrn <- "\\*\\*\\*\\*\\s+PUMPED WELL"
  length(grep(ptrn, readLines(plotfile))) > 0
}

# .wtReadPlotFileListView ------------------------------------------------------
.wtReadPlotFileListView <- structure(
  function # Read WTAQ plot file (list view)
  ### Read WTAQ plot file (list view)
  (
    file, 
    ### plot file (in \dQuote{list view}, i.e. in which data is organised in
    ### blocks being written one after the other) as produced by WTAQ model.
    dbg = FALSE
    ### if TRUE, debug messages are shown. Default: FALSE
  ) 
  {  
    # Read file without type conversion
    raw <- read.table(file, fill = TRUE, colClasses = "character", flush = TRUE,
                      col.names = paste("V", 1:5, sep = ""), 
                      na.strings = "****",
                      stringsAsFactors=FALSE)
    
    # stop if raw is empty
    if (nrow(raw) == 0) {
      stop(sprintf("Plot file \"%s\"is empty. Check the WTAQ output.", file))
    }
    
    # first and last lines of data blocks
    beg <- which(is.na(raw$V1) | raw$V1 == "OBSERVATION")
    end <- c((beg - 1)[-1], nrow(raw)) # last line of data block
    
    # initialise result data frame
    dat <- NULL
    
    # Go through data blocks
    for (i in 1:length(beg)) {
      wellname <- raw[beg[i], "V5"]
      if (wellname == "") {
        wellname <- "PW"
      }
      
      # column selection
      cs <- which(raw[beg[i]+1, ] != "") 
      
      # cut block of data and name columns
      block <- data.frame(wellname, raw[(beg[i]+2):end[i], cs], 
                          stringsAsFactors=FALSE)
      names(block) <- c("WELL", raw[(beg[i]+1), cs])
      
      if (dbg) {
        cat(sprintf("Wellname: %s\n", wellname))
        print(block)      
      }
      
      # convert character to numeric
      for (i in 2:ncol(block)) {
        block[, i] <- type.convert(block[, i])
      }
      
      # add data block to result data frame 
      dat <- rbind(dat, block)
    }  
    # Return data frame
    class(dat) <- c("wtaqResult", class(dat))
    attr(dat, "src") <- file
    
    dat
    ### data frame with same columns as contained in each block of the plot file,
    ### plus an additional column WELL containing the well ID as appearing in 
    ### the header of each data block
  }, ex = function() 
  {
    
    ### Read plot files as provided with \dQuote{sample problems} 2 and 3 in WTAQ
    ### installation files
    pfile2 <- system.file("extdata", "plt.sp2", package = "kwb.wtaq")
    pfile3 <- system.file("extdata", "plt.sp3", package = "kwb.wtaq")
    
    dat2 <- wtReadPlotFile(pfile2, logtimes = FALSE)
    dat3 <- wtReadPlotFile(pfile3, logtimes = FALSE)
    
    ### Plot CALCDD over TIME
    tr2 <- lattice::xyplot(CALCDD ~ TIME, groups = WELL, 
                  data = dat2, type = "l", auto.key = list(columns = 5),
                  main = "Example 2")
    
    tr3 <- lattice::xyplot(CALCDD ~ TIME, groups = WELL, 
                  data = dat3, type = "l", auto.key = list(columns = 5),
                  main = "Example 3")
    print(tr2)
    print(tr3)  
  }
)

# .wtReadPlotFileMatrixView ----------------------------------------------------
.wtReadPlotFileMatrixView <- structure(
  function # Read WTAQ plot file (matrix view)
  ### Read WTAQ plot file (matrix view)
  (
    plotfile, 
    ### plot file (in \dQuote{matrix view}, i.e. in which data is organised in
    ### one table only) as produced by WTAQ model.
    dbg = FALSE
    ### if TRUE, debug messages (\dQuote{Reading file...}) are shown. 
    ### Default: FALSE
  ) 
  {  
    kwb.utils::catIf(dbg, "Reading", plotfile, "...")
    
    # Read file
    tdat <- read.table(plotfile, header = FALSE, fill = TRUE, 
                       stringsAsFactors = FALSE)
    
    # Grep for rows starting with a non-numeric character (\D)
    begi <- grep("^\\D", tdat[, 1]) #c(which(tdat[, 1] == "TIME"))
    endi <- c(begi[-1]-1, nrow(tdat))
    
    dat <- NULL
    
    # Loop through data blocks
    for (i in 1:length(begi)) {
      
      # IDs of columns having a non-empty caption
      cids <- tdat[begi[i], ] != ""
      
      # Cut data block
      sdat <- tdat[(begi[i]+1):endi[i], cids]
      
      # Give column names
      cnames <- tdat[begi[i], cids]
      names(sdat) <- cnames
      
      if (is.null(dat)) {
        dat <- sdat
      } 
      else {
        # Bind columns except key columns (columns that are already in dat, 
        # e.g. TIME column)
        keycols <- which(cnames %in% intersect(names(dat), cnames))
        dat <- cbind(dat, sdat[, -keycols, drop=FALSE]) 
      }    
    }
    # Convert all columns to numeric
    for (i in 1:ncol(dat)) {
      dat[[i]] <- as.numeric(dat[[i]])
    }
    
    kwb.utils::catIf(dbg, "ok.\n")
    
    # Return data frame
    class(dat) <- c("wtaqResult", class(dat))
    attr(dat, "src") <- plotfile
    
    dat
    ### data frame corresponding with table contained in given plot file.
  }, ex = function() 
  {
    
    ### Read plot file as provided with \dQuote{sample problem 1} in WTAQ
    ### installation files
    pfile1 <- system.file("extdata", "plt.sp1", package = "kwb.wtaq")
    dat1 <- .wtReadPlotFileMatrixView(pfile1)
    
    ### Plot HDPW + HDOB1 + HDOB2 + HDOB3 + HDOB4 over dimensionless time (TDRDSQ)
    tr1 <- lattice::xyplot(HDPW + HDOB1 + HDOB2 + HDOB3 + HDOB4 ~ TDRDSQ, 
                  data = dat1, type = "l", auto.key = list(columns = 5), 
                  main = "Example 1")    
    print(tr1)
  }
)
