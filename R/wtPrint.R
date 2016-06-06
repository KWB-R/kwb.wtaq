# print.wtaqConfiguration ------------------------------------------------------
print.wtaqConfiguration <- function # Print WTAQ configuration
### Print WTAQ configuration
(
  x, 
  ...
) 
{
  .printWtaqConfGeneral(x$general)
  .printWtaqConfAquifer(x$aquifer)
  .printWtaqConfDrainage(x$drainage)
  .printWtaqConfTimes(x$times)
  .printWtaqConfSolution(x$solution)
  if (!is.null(x$pumpwell)) {
    .printWtaqConfPumpwell(x$pumpwell)  
  }  
  if (!is.null(x$obswells)) {
    for (i in seq(1, by = 1, length.out=length(x$obswells))) {
      .printWtaqConfObswell(x$obswells[[i]])
    }
  }
}

# .wtIdraToString(): conf$drainage$idra ----------------------------------------
.wtIdraToString <- function
(
  idra
) 
{
  if (idra == 0) {
    "Instantaneous drainage"
  } else if (idra == 1) {
    "Gradual drainage"
  } else if (idra == 2) {
    "Drainage with unsaturated-zone characterization"
  } else {
    sprintf("Unknown (idra = %d)", idra)
  }
}

# .wtItsToString(): conf$times$its ---------------------------------------------
.wtItsToString <- function
(
  its
) 
{
  if (its == 0) {
    "Log-cycle time steps"
  } else if (its == 1) {
    "User-specified times"
  } else {
    sprintf("Unknown (its = %d)", its)
  }
}

# .wtIsolnToString(): conf$solution$isoln --------------------------------------
.wtIsolnToString <- function
(
  isoln
) 
{
  if (isoln == 1) {
    "Stehfest algorithm"
  } else if (isoln == 2) {
    "de Hoog algorithm"
  } else {
    sprintf("Unknown (isoln = %d)", isoln)
  }
}

# .wtIpwsToString(): conf$pumpwell$ipws ----------------------------------------
.wtIpwsToString <- function
(
  ipws
) 
{
  if (ipws == 0) {
    "Partially penetrating pumped well"
  } else if (ipws == 1) {
    "Fully penetrating pumped well"
  } else {
    sprintf("Unknown (ipws = %d)", ipws)
  }
}

# .wtIpwdToString(): conf$pumpwell$ipwd ----------------------------------------
.wtIpwdToString <- function
(
  ipwd
) 
{
  if (ipwd == 0) {
    "Infinitesimal diameter (line-source theory)"
  } else if (ipwd == 1) {
    "Finite diameter"
  } else {
    sprintf("Unknown (ipwd = %d)", ipwd)
  }
}

# .wtIowsToString(): conf$obswell$iows -----------------------------------------
.wtIowsToString <- function
(
  iows
) 
{
  if (iows == 0) {
    "Partially penetrating observation well"
  } else if (iows == 1) {
    "Fully penetrating observation well"
  } else if (iows == 2) {
    "Observation piezometer"
  } else {
    sprintf("Unknown (iows = %d)", iows)
  }
}

# .printWtaqConfGeneral(): conf$general ----------------------------------------
.printWtaqConfGeneral <- function # Print WTAQ configuration$general
### Print WTAQ configuration$general
(
  x
) 
{
  cat("General\n")
  cat("- Title: ", x$title, "\n")
  cat("- Format:", x$format, "\n")  
}

# .printWtaqConfAquifer(): conf$aquifer ----------------------------------------
.printWtaqConfAquifer <- function # Print WTAQ configuration$aquifer
### Print WTAQ configuration$aquifer
(
  x
)  
{
  cat("\nAquifer\n")
  cat("- Type:", x$aqtype, "\n")
  cat(sprintf("- Thickness (bb) ............: %f (length)\n", x$bb))
  cat(sprintf("- Horiz. hydr. conduct. (hkr): %f (length/time)\n", x$hkr))
  cat(sprintf("- Vert.  hydr. conduct. (hkz): %f (length/time)\n", x$hkz))
  cat(sprintf("- Specific storage (ss) .....: %f (1/length)\n", x$ss))
  cat(sprintf("- Specific yield (sy) .......: %f\n", x$sy))
}

# .printWtaqConfDrainage(): conf$drainage --------------------------------------
.printWtaqConfDrainage <- function # Print WTAQ configuration$drainage
### Print WTAQ configuration$drainage
(
  x
) 
{
  cat("\nDrainage\n")  
  cat(sprintf("- Type: %s (idra = %d)\n", .wtIdraToString(x$idra), x$idra))
  if (x$idra == 1) {
    cat(sprintf("- Drainage constants (alpha; 1/time): %s\n",
                paste(x$alpha, collapse = ", ")))
  } else if (x$idra == 2) {
    cat(sprintf("- Soil-moisture retention exponent (acc) .................: %8.2f (1/length)\n", 
                x$acc))
    cat(sprintf("- Relative hydraulic-conduct. exponent (akk) .............: %8.2f (1/length)\n", 
                x$akk))
    cat(sprintf(paste("- Initial unsat.-zone thickn. above",
                      "capillary fringe (amm): %8.2f (length)\n"), x$amm))
    cat(sprintf(paste("- Unsat.-zone thickn. above capillary fringe",
                      "(axmm) ......: %8.2f (length)\n"), x$amm))
  }  
}

# .printWtaqConfTimes(): conf$times --------------------------------------------
.printWtaqConfTimes <- function # Print WTAQ configuration$times
### Print WTAQ configuration$times
(
  x
) 
{
  cat("\nTimes\n")
  cat(sprintf("- Type: %s (its = %d)\n", .wtItsToString(x$its), x$its)) 
  if (x$its == 0) {
    cat(sprintf("- Largest value of time (tlast) .............: %f\n", x$tlast))
    cat(sprintf("- # logarithmic cycles (nlc) ................: %d\n", x$nlc))
    cat(sprintf("- # equally spaced times per log. cycle (nox): %d\n", x$nox))
  }
}

# .printWtaqConfSolution(): conf$solution --------------------------------------
.printWtaqConfSolution <- function # Print WTAQ configuration$solution
### Print WTAQ configuration$solution
(
  x
) 
{
  cat("\nSolution\n")
  cat(sprintf("- Type: %s (isoln = %d)\n", .wtIsolnToString(x$isoln), x$isoln)) 
  txt <- list(
    rerrnr  = sprintf("- Rel. err. for Newton-Raphson iter. (rerrnr) ...................: %f\n",
                      x$rerrnr),
    rerrsum = sprintf("- Rel. err. for finite summations (rerrsum) .....................: %f\n", 
                      x$rerrsum),
    nmax    = sprintf("- Max. # terms permit. in finite summations (nmax) ..............: %d\n", 
                      x$nmax),
    ntms    = sprintf("- Factor to determ. # terms in finite summations (ntms) .........: %d\n", 
                      x$ntms),
    ns      = sprintf("- # of terms used in the Stehfest algorith (ns) .................: %d\n",
                      x$ns),
    error   = sprintf("- Rel. err. sought for accur. of the num. invers. (error) .......: %f\n",
                      x$error),
    nnn     = sprintf("- # terms in summ. of Fourier series for inv. Lapl. transf. (nnn): %d\n", 
                      x$nnn),
    method  = sprintf("- Method used to accelerate convergence of Fourier series .......: %d\n", 
                      x$method)
  )
  if (x$isoln == 1) {
    cat(txt$rerrnr, txt$rerrsum, txt$nmax, txt$ntms, txt$ns, sep  ="")
  } else if (x$isoln == 2) {
    cat(txt$rerrnr, txt$error, txt$ntms, txt$nnn, txt$method, sep = "")
  }
}

# .wtPrintTimeSeries -----------------------------------------------------------
.wtPrintTimeSeries <- function
(
  x
) 
{
  if (!is.null(x)) {
    cat(sprintf("- Predefined Times (t)%s:\n", 
                ifelse(ncol(x) > 1, "/drawdown measurements (dd)", "")))
    print(x)
  } else {
    cat("- No times/drawdown measurements defined.\n")
  }
}

# .printWtaqConfPumpwell(): conf$pumpwell --------------------------------------
.printWtaqConfPumpwell <- function # Print WTAQ configuration$pumpwell
### Print WTAQ configuration$pumpwell
(
  x
) 
{
  cat(sprintf("\nPumped Well (\"%s\")\n", x$pwname))
  cat(sprintf("- Type: %s (ipws = %d)\n", .wtIpwsToString(x$ipws), x$ipws))
  cat(sprintf("- Type of diameter: %s (ipwd = %d)\n", .wtIpwdToString(x$ipwd), x$ipwd))
  cat(sprintf("- Pumping rate (qq) .......................................: %f (length^3/time)\n",
              x$qq))
  cat(sprintf("- Radius of screen (rw) ...................................: %f (length)\n", 
              x$rw))
  cat(sprintf("- Inside radius where levels change during pumping (rc) ...: %f (length)\n", 
              x$rc))
  cat(sprintf("- Top of aquifer/initial water table to screen top (zpd) ..: %f (length)\n", 
              x$zpd))
  cat(sprintf("- Top of aquifer/initial water table to screen bottom (zpl): %f (length)\n", 
              x$zpl))
  cat(sprintf("- Well-bore skin parameter (sw) ...........................: %f \n", 
              x$sw))
  .wtPrintTimeSeries(x$tspw)
}

# .printWtaqConfObswell(): conf$obswell ----------------------------------------
.printWtaqConfObswell <- function # Print WTAQ configuration$obswell
### Print WTAQ configuration$obswell
(
  x
) 
{
  
  txt <- list(
    obname = sprintf("\nObservation Well \"%s\"\n", x$obname),
    iows   = sprintf("- Type: %s (iows = %d)\n", .wtIowsToString(x$iows), x$iows),
    idpr   = sprintf("- Delayed response: %s (idpr = %d)\n", 
                     ifelse(x$idpr, "yes", "no"), x$idpr),
    r      = sprintf("- Radial dist. from axis of pumped well to obs. well (r) .....: %f (length)\n",
                     x$r),
    zp     = sprintf("- Top of aquifer/initial water table to piezometer center (zp): %f (length)\n", 
                     x$zp),
    z1     = sprintf("- Top of aquifer/initial water table to screen top (z1) ......: %f (length)\n", 
                     x$z1),
    z2     = sprintf("- Top of aquifer/initial water table to screen bottom (z2) ...: %f (length)\n", 
                     x$z2), 
    rp     = sprintf("- Inside radius of standpipe where levels change (rp) ........: %f (length)\n", 
                     x$rp),
    xll    = sprintf("- Length of screened interval (xll) ..........................: %f (length)\n", 
                     x$xll))
  
  cat(txt$obname, txt$iows, txt$idpr, txt$r, sep = "")
  if (x$iows == 2) {
    cat(txt$zp)
  } 
  else {
    cat(txt$z1, txt$z2, sep = "") 
  }  
  cat(txt$rp, txt$xll, sep = "")
  .wtPrintTimeSeries(x$tsobs)  
}

