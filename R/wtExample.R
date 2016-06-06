# wtConfigurationExample2 ------------------------------------------------------
wtConfigurationExample2 <- structure(
  function # WTAQ configuration representing WTAQ example 2
  ### WTAQ configuration corresponding to sample problem 2 of WTAQ distribution
  (
  )
  {
    ##seealso<< \code{\link{wtConfigurationExample3}, \link{wtConfigure}}
    gen <- wtConfigureGeneral(
      title="Sample problem 2, using dimensional format (seconds, meters).",
      format="DIMENSIONAL")
    
    aqf <- wtConfigureAquifer(
      aqtype="WATER TABLE", 
      bb=10.0, hkr=1.0e-4, hkz=0.5e-4, ss=2.0e-5, sy=2.0e-01)
    
    drn <- wtConfigureDrainage(idra=0)
    
    tms <- wtConfigureTimes(its=1, tlast=0.0e0, nlc=0, nox=0)
    
    sol <- wtConfigureSolution(
      isoln=1, rerrnr=1.0e-10, rerrsum=0.0, nmax=0, ntms=30, ns=8)
    
    pw <- wtConfigurePumpwell(
      irun=1, ipws=0, ipwd=1, ipump=1,
      qq=2.0e-3, rw=0.1, rc=0.1, zpd=5.0, zpl=10.0, sw=0.0,
      tspw=data.frame(
        t = c(9.28e00, 2.00e01, 4.31e01, 9.28e01, 2.00e02, 4.31e02, 9.28e02, 
              2.00e03, 4.31e03, 9.28e03, 2.00e04, 4.31e04, 9.28e04, 2.00e05),
        dd = c(0.51, 0.97, 1.64, 2.31, 2.65, 2.71, 2.71, 2.72, 2.74, 2.76, 
               2.81, 2.89, 2.98, 3.09)))
    
    ow1 <- wtConfigureObservationWell(
      irun=1, obname="PD1", iows=2, idpr=0,
      r=3.16, z1=0.0, z2=0.0, zp=7.5, rp=0.0, xll=0.0,
      tsobs=data.frame(
        t= c(9.28e00, 2.00e01, 4.31e01, 9.28e01, 2.00e02, 4.31e02, 9.28e02,
             2.00e03, 4.31e03, 9.28e03, 2.00e04, 4.31e04, 9.28e04, 2.00e05),
        dd=c(0.09, 0.204, 0.384, 0.568, 0.662, 0.68, 0.683, 0.69, 0.703, 0.729, 
             0.775, 0.847, 0.942, 1.052)))
    
    ow2 <- wtConfigureObservationWell(
      irun=1, obname="PD2", iows=2, idpr=0,
      r=31.6, z1=0.0, z2=0.0, zp=7.5, rp=0.0, xll=0.0,
      tsobs=data.frame(
        t=c(9.28e00, 2.00e01, 4.31e01, 9.28e01, 2.00e02, 4.31e02, 9.28e02, 
            2.00e03, 4.31e03, 9.28e03, 2.00e04, 4.31e04, 9.28e04, 2.00e05),
        dd=c(0, 5e-04, 0.0028, 0.0071, 0.0099, 0.0105, 0.011, 0.0119, 0.014, 
             0.0186, 0.0292, 0.0524, 0.0983, 0.1722)))
    
    ow3 <- wtConfigureObservationWell(
      irun=1, obname="PS1", iows=2, idpr=0,
      r=3.16, z1=0.0, z2=0.0, zp=1.0, rp=0.0, xll=0.0,
      tsobs=data.frame(
        t=c(9.28e00, 2.00e01, 4.31e01, 9.28e01, 2.00e02, 4.31e02, 9.28e02, 
            2.00e03, 4.31e03, 9.28e03, 2.00e04, 4.31e04, 9.28e04, 2.00e05),
        dd=c(0.007, 0.019, 0.039, 0.06, 0.072, 0.077, 0.084, 0.098, 0.126, 0.177, 
             0.258, 0.365, 0.487, 0.612)))
    
    ow4 <- wtConfigureObservationWell(
      irun=1, obname="PS2", iows=2, idpr=0,
      r=31.6, z1=0.0, z2=0.0, zp=1.0, rp=0.0, xll=0.0,
      tsobs=data.frame(
        t=c(9.28e00, 2.00e01, 4.31e01, 9.28e01, 2.00e02, 4.31e02, 9.28e02, 
            2.00e03, 4.31e03, 9.28e03, 2.00e04, 4.31e04, 9.28e04, 2.00e05),
        dd=c(0, 1e-04, 5e-04, 0.0012, 0.0017, 0.0019, 0.0022, 0.0028, 0.0042, 
             0.0075, 0.016, 0.038, 0.084, 0.162)))
    
    # Return configuration of class "wtaqConfiguration"
    x <- wtConfigure(
      general=gen, aquifer=aqf, drainage=drn, times=tms, solution=sol, 
      pumpwell=pw, obswells=list(ow1, ow2, ow3, ow4))
    
    #class(x) <- "wtaqConfiguration"
    x
    
    ### list with elements \emph{general}, \emph{aquifer}, \emph{drainage},
    ### \emph{times}, \emph{solution}, \emph{pumpwell}, \emph{obswells},
    ### representing a full WTAQ configuration. 
    
  }, ex = function() {
    # Get configuration of sample problem 2 of WTAQ distribution
    cnf <- wtConfigurationExample2()
    
    # Print formatted output of configuration
    print(cnf)
    
    # Plot wellfield profile of configuration
    wtPlotConfiguration(cnf)
  })

# wtConfigurationExample3-------------------------------------------------------
wtConfigurationExample3 <- structure(
  function # WTAQ configuration representing WTAQ example 3
  ### WTAQ configuration corresponding to sample problem 3 of WTAQ distribution
  (
  )
  {
    ##seealso<< \code{\link{wtConfigurationExample2}, \link{wtConfigure}}
    
    gen <- wtConfigureGeneral(
      title="Sample problem 3, with Borden site conditions (minutes, meters).",
      format="DIMENSIONAL")
    
    aqf <- wtConfigureAquifer(
      aqtype="WATER TABLE", 
      bb=6.2, hkr=4.1e-03, hkz=1.74e-03, ss=3.76e-05, sy=0.25)
    
    drn <- wtConfigureDrainage(
      idra=2, alpha=c(), acc=5.0, akk=31.7, amm=100.0, axmm=10.0)
    
    tms <- wtConfigureTimes(
      its=1, tlast=0.0, nlc=0, nox=0)
    
    sol <- wtConfigureSolution(
      isoln=2, rerrnr=1.0e-10, error=1.0e-4, ntms=30, nnn=6, method=3)
    
    pw <- wtConfigurePumpwell(
      irun=1, ipws=0, ipwd=1, ipump=1,
      qq=40.0e-03, 
      rw=0.065, rc=0.065, zpd=2.55, zpl=6.20, sw=1.74,
      tspw=data.frame(
        t = c(  0.03730,    0.07470,     0.11200,    0.14930,    0.22400,
                0.33600,    0.48530,     0.67200,    1.00650,    1.52700,
                2.22730,    3.27550,     4.84430,    6.79580,   10.11200,
                15.07370,   22.49780,    31.73280,   47.42450,   66.94330,
                100.10850,  149.73130,   223.97930,  316.33450,  473.25770,
                668.44980, 1000.10530,  1496.34100, 2238.82630, 3162.38270,
                4697.07000, 6857.07000, 10457.07000)))
    
    ow1 <- wtConfigureObservationWell(
      irun=1, obname="WD1A", iows=2, idpr=1,
      r=1.51, z1=0.0, z2=0.0, zp=0.94, rp=0.025, xll=0.35,
      tsobs=data.frame(
        t= c(    0.33600,    0.48530,     0.67200,    1.00650,    1.52700,
                 2.22730,    3.27550,     4.84430,    6.79580,   10.11200,
                 15.07370,   22.49780,    31.73280,   47.42450,   66.94330,
                 100.10850,  149.73130,   223.97930,  316.33450,  473.25770,
                 668.44980, 1000.10530,  1496.34100, 2238.82630, 3162.38270,
                 4697.07000, 6857.07000, 10457.07000)))
    
    ow2 <- wtConfigureObservationWell(
      irun=1, obname="WD2A", iows=2, idpr=1,
      r=5.07, z1=0.0, z2=0.0, zp=0.89, rp=0.025, xll=0.35,
      tsobs=data.frame(
        t=c(    0.22400,    0.33600,    0.48530,     0.67200,    1.00650,
                1.52700,    2.22730,    3.27550,     4.84430,    6.79580,
                10.11200,   15.07370,   22.49780,    31.73280,   47.42450,
                66.94330,  100.10850,  149.73130,   223.97930,  316.33450,
                473.25770,  668.44980, 1000.10530,  1496.34100, 2238.82630,
                3162.38270, 4697.07000, 6857.07000, 10457.07000)))
    
    ow3 <- wtConfigureObservationWell(
      irun=1, obname="WD4A", iows=2, idpr=1,
      r=15.05, z1=0.0, z2=0.0, zp=0.84, rp=0.025, xll=0.35,
      tsobs=data.frame(
        t=c(    0.48750,     0.67500,    1.02520,    1.49300,    2.19330,
                3.24150,     4.81030,    6.76180,   10.07800,   15.03970,
                22.46380,    31.69880,   47.39050,   66.90930,  100.07450,
                149.69730,   223.94530,  316.30050,  473.22370,  668.41580,
                1000.07130,  1496.30700, 2238.79230, 3162.34870, 4697.03600,
                6857.03600, 10457.03600)))
    
    ow4 <- wtConfigureObservationWell(
      irun=1, obname="P17", iows=2, idpr=1,
      r=5.15,  z1=0.0, z2=0.0, zp=2.69, rp=0.025, xll=0.35,
      tsobs=data.frame(
        t=c(    0.03750,    0.07500,     0.11250,    0.15000,    0.22500,
                0.33750,    0.48750,     0.67500,    1.02520,    1.49300,
                2.19330,    3.24150,     4.81030,    6.76180,   10.07800,
                15.03970,   22.46380,    31.69880,   47.39050,   66.90930,
                100.07450,  149.69730,   223.94530,  316.30050,  473.22370,
                668.41580, 1000.07130,  1496.30700, 2238.79230, 3162.34870,
                4697.03600, 6857.03600, 10457.03600)))
    
    ow5 <- wtConfigureObservationWell(
      irun=1, obname = "P4", iows=2, idpr=1,
      r=15.36, z1=0.0, z2=0.0, zp=2.32, rp=0.0175, xll=0.35,
      tsobs=data.frame(
        t=c(    0.67500,    1.02520,    1.49300,    2.19330,    3.24150,
                4.81030,    6.76180,   10.07800,   15.03970,   22.46380,
                31.69880,   47.39050,   66.90930,  100.07450,  149.69730,
                223.94530,  316.30050,  473.22370,  668.41580, 1000.07130,
                1496.30700, 2238.79230, 3162.34870, 4697.03600, 6857.03600,
                10457.03600)))
    
    ow6 <- wtConfigureObservationWell(
      irun=1, obname="P14", iows=2, idpr=1,
      r=1.51, z1=0.0, z2=0.0, zp=4.57, rp=0.025, xll=0.35,
      tsobs=data.frame(
        t=c(    0.14930,    0.22400,    0.33600,    0.48530,     0.67200,
                1.00650,    1.52700,    2.22730,    3.27550,     4.84430,
                6.79580,   10.11200,   15.07370,   22.49780,    31.73280,
                47.42450,   66.94330,  100.10850,  149.73130,   223.97930,
                316.33450,  473.25770,  668.44980, 1000.10530,  1496.34100,
                2238.82630, 3162.38270, 4697.07000, 6857.07000, 10457.07000)))
    
    ow7 <- wtConfigureObservationWell(
      irun=1, obname="P5",  iows=2, idpr=0,
      r=30.19, z1=0.0,  z2=0.0, zp=4.30, rp=0.025, xll=0.35,
      tsobs=data.frame(
        t = c( 480.00000,  1080.00000, 1950.00000, 3420.00000, 4920.00000, 
               6750.00000, 10530.00000)))
    
    # Return configuration of class "wtaqConfiguration"
    x <- wtConfigure(
      general = gen, aquifer = aqf, drainage = drn, times = tms, solution = sol, 
      pumpwell = pw, obswells = list(ow1, ow2, ow3, ow4, ow5, ow6, ow7))
    
    #class(x) <- "wtaqConfiguration"
    
    x
    ### list with elements \emph{general}, \emph{aquifer}, \emph{drainage},
    ### \emph{times}, \emph{solution}, \emph{pumpwell}, \emph{obswells},
    ### representing a full WTAQ configuration. 
    
  }, ex = function() {
    # Get configuration of sample problem 3 of WTAQ distribution
    cnf <- wtConfigurationExample3()
    
    # Print formatted output of configuration
    print(cnf)
    
    # Plot wellfield profile of configuration
    wtPlotConfiguration(cnf)
  })

