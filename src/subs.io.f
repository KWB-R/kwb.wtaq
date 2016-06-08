C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE OFILE                    *
C      *                                                      *
C      *        VERSION 2.1 CURRENT AS OF 06/29/2012          *
C      *                                                      *
C       ******************************************************
C
C10-SUBROUTINE OFILE OPENS INPUT, RESULT, AND PLOT FILES
C
      SUBROUTINE OFILE(IPLOT)
C---SPECIFICATIONS
      CHARACTER*50 IFNAME, OFNAME, PFNAME
      INTEGER IN, IO, IP, N, IPLOT
      COMMON /IOUNIT/ IN,IO,IP,ILOG
C
      IN = 15
      IO = 16
      IP = 17
      ILOG = 18
C
C---Version 1.1: add WTAQ banner:
      WRITE(*,8)
C
C---READ INPUT, RESULT, AND PLOT FILE NAMES
   10 WRITE(*,2)
      READ(*,5) IFNAME
      N=LENCHR(IFNAME)
      IF(N.LT.1) GO TO 10
      OPEN(IN,FILE=IFNAME,STATUS='OLD')
C
   11 WRITE(*,3)
      READ(*,5) OFNAME
      N=LENCHR(OFNAME)
      IF(N.LT.1) GO TO 11
      OPEN(IO,FILE=OFNAME)
C
      WRITE(*,4)
      READ(*,5) PFNAME
      N=LENCHR(PFNAME)
      IF(N.LT.1)THEN
       IPLOT=0
       WRITE(*,6)
      ELSE
       IPLOT=1
       OPEN(IP,FILE=PFNAME)
      ENDIF
C
      OPEN(ILOG, FILE='WTAQ.LOG')
      WRITE(ILOG,9)
C
      WRITE(*,7)
C
C---FORMAT STATEMENTS
    2 FORMAT(3X,'Enter name of file containing input data:')
    3 FORMAT(3X,'Enter name of file for program results:')
    4 FORMAT(3X,'Enter name of plot file (return for no plot file):')
    5 FORMAT(A50)
    6 FORMAT(/,3X,'No plot file specified. Results will not be written',
     1' to a plot file.')
    7 FORMAT(/,3X,'File names have been read.')
C---Version 1.1: add FORMAT statement:
    8 FORMAT(10X,'       WTAQ version 2.1, June 2012',/)
    9 FORMAT('WTAQ RUN-TIME LOG FILE: A LINE IDENTIFYING EACH WELL OR',
     2 ' PIEZOMETER IS',/,' WRITTEN TO THIS FILE. IF NO WARNING',
     3 ' MESSAGES ARE PRINTED, THEN THE',/,' CALCULATIONS MADE FOR',
     4 ' THE WELL OR PIEZOMETER MET ALL OF THE INTERNAL',/,
     5 ' ACCURACY TESTS.',/,/,
     6 ' (DEFINITIONS OF WARNINGS PROVIDED AT END OF FILE)',/)
C
C---RETURN AND END
      RETURN
      END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION LENCHR                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C11-FUNCTION LENCHR CALCULATES THE LENGTH OF A CHARACTER STRING
C   (CODE FROM R. STEVE REGAN, USGS)
C
      INTEGER FUNCTION LENCHR
     I                       ( STRING )
C
      CHARACTER*(*) STRING
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - Input string to determine length of
C
C     + + + LOCAL VARIABLES + + +
      INTEGER MAX, I
C
C     + + + INTRINSICS + + +
      INTEGER LEN
      INTRINSIC LEN
C
C     + + + END SPECIFICATIONS + + +
C
      MAX = LEN(STRING)
      DO 10 I = MAX, 1, -1
        IF (STRING(I:I).NE.' ') THEN
          LENCHR = I
          GOTO 20
        ENDIF
   10 CONTINUE
      LENCHR = 0
C
   20 RETURN
      END
C
C
C       ******************************************************
C      *                                                      *
C      *                 SUBROUTINE DATAPREP                  *
C      *                                                      *
C      *        VERSION 2.1 CURRENT AS OF 06/29/2012          *
C      *                                                      *
C       ******************************************************
C
C12-SUBROUTINE DATAPREP READS MOST INPUT DATA FROM THE INPUT FILE,
C   WRITES THIS DATA TO THE RESULT FILE, AND PREPARES INPUT DATA
C   FOR USE IN MAIN PROGRAM
      SUBROUTINE DATAPREP
C---SPECIFICATIONS
C
C---THE PARAMETER STATEMENT BELOW SPECIFIES THE FOLLOWING PROGRAM
C   DIMENSIONS:
C    IMAXX1 = MAXIMUM NUMBER OF TIME-DRAWDOWN CURVES TO BE GENERATED
C             (THE FIRST CURVE IS ALWAYS THE PUMPED WELL, EVEN WHEN
C             CURVES ARE NOT CALCULATED AT THE PUMPED WELL)
C    IMAXX2 = MAXIMUM NUMBER OF USER SPECIFIED TIMES OR MEASURED
C             DRAWDOWNS AT THE PUMPED WELL OR OBSERVATION WELLS
C
      PARAMETER (IMAXX1=25,IMAXX2=200)
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION ALPHA(5),GAMMA(5)
      DIMENSION TIMEPW(IMAXX2),XMEASPW(IMAXX2)
      CHARACTER STRING*80,TITLE*70,FORMAT*25,AQTYPE*11,
     2 FORMATTXT*27,AQTYPETXT*20
C---COMMON STATEMENTS
      COMMON /IOUNIT/ IN,IO,IP,ILOG
C---WTAQ VERSION 2
      COMMON /PAR1/ IPWD,IRUN,IPWS,NOBWC,IOWS,IDPR,IPUMP
      COMMON /PAR2/ NGAMMA,IDRA,NS,KK,NMAX,NTMS,ISOLN,NNN,METHOD
C
      COMMON /PAR3/ IFORMAT,IAQ,NLC,NOX
      COMMON /PAR4/ ITS,IMEAS,NTSPW
      COMMON /PAR5/ BB,HKR,XKD,SS,SY,RW,QQ
      COMMON /PAR6/ BETAW,SIGMA,GAMMA
      COMMON /PAR7/ RERRNR,RERRSUM,ERROR,TDLAST,TLAST
      COMMON /PAR8/ R,ZP,Z1,Z2,WDP
      COMMON /PAR9/ V(20),XLN2,EXPMAX
      COMMON /PAR10/ RD,ZD,ZD1,ZD2
      COMMON /PAR11/ XLD,XDD,WD,SW
      COMMON /PAR12/ TIMEPW,XMEASPW
C---WTAQ VERSION 2
      COMMON /PAR13/ ADC,ADK,ADM,BETA,XMAXDM
C
C12a-WRITE PROGRAM BANNER TO RESULT FILE
      CALL BANNER(IO)
C
C
C12b-READ AND PREPARE INPUT DATA
C
C---READ TITLE
      READ(IN,'(A70)') STRING
      ILINE=1
      READ(STRING,100,ERR=1000) TITLE
C
C---READ AND TEST TYPE OF ANALYSIS FORMAT
      IFORMAT=2
      READ(IN,'(A25)') STRING
      ILINE=2
      READ(STRING,101,ERR=1000) FORMAT 
      IF(FORMAT.EQ.'TYPE CURVE')IFORMAT=0
      IF(FORMAT.EQ.'DIMENSIONAL')IFORMAT=1
C
C---IF IFORMAT STILL EQUALS 2, UNRECOGNIZED FORMAT; STOP
      IF(IFORMAT.EQ.2)THEN
        WRITE(IO,102)
        STOP
      ENDIF
C
C---READ AND TEST AQUIFER TYPE
      IAQ=2
      READ(IN,'(A11)') STRING
      ILINE=3
      READ(STRING,103,ERR=1000) AQTYPE
      IF(AQTYPE.EQ.'CONFINED')IAQ=0
      IF(AQTYPE.EQ.'WATER TABLE')IAQ=1
C
C---IF IAQ STILL EQUALS 2, UNRECOGNIZED AQUIFER TYPE; STOP
      IF(IAQ.EQ.2)THEN
        WRITE(IO,104)
        STOP
      ENDIF
C
C---READ AND TEST AQUIFER PROPERTIES
      READ(IN,'(A80)') STRING
      ILINE=4
      IF(IFORMAT.EQ.0)READ(STRING,*,ERR=1000) BB, XKD, SIGMA
      IF(IFORMAT.GE.1)READ(STRING,*,ERR=1000) BB, HKR, HKZ, SS, SY
C
      IF(BB.LE.0.0D0)THEN
       WRITE(IO,105)
       STOP
      ENDIF
      IF(IFORMAT.EQ.0)THEN
C---SIGMA.LE.0.0D0 changed to LT.0.0D0 for version 1.3
       IF(SIGMA.LT.0.0D0.OR.XKD.LE.0.0D0)THEN
        WRITE(IO,105)
        STOP
       ENDIF
      ENDIF
      IF(IFORMAT.GE.1)THEN
       IF(HKR.LE.0.0D0.OR.HKZ.LE.0.0D0)THEN
        WRITE(IO,105)
        STOP
       ENDIF
       IF(SS.LE.0.0D0)THEN
        WRITE(IO,105)
        STOP
       ENDIF
       AT=HKR*BB
       XKD=HKZ/HKR
       ASC=SS*BB
       IF(IAQ.EQ.1)THEN
        IF(SY.LE.0.0D0)THEN
         WRITE(IO,105)
         STOP
        ENDIF
        SIGMA=ASC/SY
       ENDIF
      ENDIF
C
C---SIGMA = 0 FOR CONFINED AQUIFERS
      IF(IAQ.EQ.0)SIGMA=0.0D0
C
C---READ AND TEST DRAINAGE PROPERTIES
      READ(IN,'(A80)') STRING
      ILINE=5
      IF(IFORMAT.EQ.0)READ(STRING,*,ERR=1000)IDRA, NGAMMA
      IF(IFORMAT.GE.1)READ(STRING,*,ERR=1000)IDRA, NALPHA
C
      IF(IDRA.LT.0.OR.IDRA.GT.2)THEN
       WRITE(IO,106)
       STOP
      ENDIF
      IF(NGAMMA.LT.0.OR.NALPHA.LT.0)THEN
       WRITE(IO,106)
       STOP
      ENDIF
      IF(NGAMMA.GT.5.OR.NALPHA.GT.5)THEN
       WRITE(IO,106)
       STOP
      ENDIF
C
      IF(IDRA.EQ.0)NGAMMA=0
      IF(IDRA.EQ.0)NALPHA=0
      IF(IAQ.EQ.0)IDRA=0
      IF(IAQ.EQ.0)NGAMMA=0
      IF(IAQ.EQ.0)NALPHA=0
C
C---READ DRAINAGE OR UNSATURATED-ZONE PARAMETERS
      READ(IN,'(A80)')STRING
      ILINE=6
C---IF IDRA=1, READ DRAINAGE PARAMETERS
      IF(IDRA.EQ.1)THEN
       IF(IFORMAT.EQ.0)READ(STRING,*,ERR=1000)(GAMMA(I),I=1,NGAMMA)
       IF(IFORMAT.GE.1)READ(STRING,*,ERR=1000)(ALPHA(I),I=1,NALPHA)
C
C12c-CONVERT DIMENSIONAL ALPHAS TO DIMENSIONLESS GAMMAS
       IF(IFORMAT.GE.1)THEN
        DO 5 I=1,NALPHA
         GAMMA(I)=ALPHA(I)*BB*SY/HKZ
  5     CONTINUE
        NGAMMA=NALPHA
       ENDIF
       DO 10 I=1,NGAMMA
        IF(GAMMA(I).LT.0.0D0)THEN
         WRITE(IO,106)
         STOP
        ENDIF
 10     CONTINUE
      ENDIF
C
C---WTAQ VERSION 2
C---IF IDRA=2, READ AND TEST UNSATURATED-ZONE PARAMETERS
      IF(IDRA.EQ.2)THEN
       READ(STRING,*,ERR=1000) ACC, AKK, AMM, AXMM
       IF(ACC.LT.0.0D0)THEN
         WRITE(IO,200)
         STOP
       ENDIF
       IF(AKK.LT.0.0D0)THEN
         WRITE(IO,200)
         STOP
       ENDIF
       IF(AMM.LT.0.0D0)THEN
         WRITE(IO,200)
         STOP
       ENDIF
       IF(AXMM.LT.0.0D0)THEN
         WRITE(IO,200)
         STOP
       ENDIF
       IF(ACC.GT.AKK)THEN
         WRITE(IO,214)
         STOP
       ENDIF
C12c-CONVERT DIMENSIONAL UNSATURATED-ZONE PARAMETERS TO DIMENSIONLESS
C      VALUES
       ADC=ACC*BB
       ADK=AKK*BB
       ADM=AMM/BB
       XMAXDM=AXMM/BB
      ENDIF
C
C---READ AND TEST TIME-STEP INFORMATION
      IF(IFORMAT.LE.1)THEN
       READ(IN,'(A80)') STRING
       IF(IFORMAT.EQ.0)THEN
        ILINE=7
        READ(STRING,*,ERR=1000) TDLAST, NLC, NOX
       ENDIF
       IF(IFORMAT.EQ.1)THEN
        ILINE=8
        READ(STRING,*,ERR=1000) ITS, IMEAS
        READ(IN,'(A80)') STRING
        ILINE=9
        READ(STRING,*,ERR=1000) TLAST, NLC, NOX
       ENDIF
      ENDIF
C
C---IF IFORMAT = 0 (TYPE CURVE), LOG-TYPE ANALYSIS. NO USER-
C    SPECIFIED TIMES OR MEASURED DRAWDOWN DATA
      IF(IFORMAT.EQ.0)THEN
       ITS=0
       IMEAS=0
      ENDIF
C
      IF(TDLAST.LT.0.0D0.OR.TLAST.LT.0.0D0)THEN
       WRITE(IO,107)
       STOP
      ENDIF
      IF(NLC.LT.0.OR.NOX.LT.0)THEN
       WRITE(IO,107)
       STOP
      ENDIF
      IF(ITS.LT.0.OR.IMEAS.LT.0)THEN
       WRITE(IO,107)
       STOP
      ENDIF

C---WTAQ VERSION 2
C---READ AND TEST SOLUTION TYPE AND SOLUTION VARIABLES
      READ(IN,'(A80)') STRING
      ILINE=10
      READ(STRING,*,ERR=1000) ISOLN
      IF(ISOLN.LT.1.OR.ISOLN.GT.2)THEN
       WRITE(IO,201)
       STOP
      ENDIF
      IF(IAQ.EQ.0)THEN
       IF(ISOLN.NE.1)THEN
        WRITE(IO,209)
        STOP
       ENDIF
      ENDIF
      IF(IAQ.EQ.1)THEN
       IF(IDRA.EQ.2)THEN
        IF(ISOLN.NE.2)THEN
         WRITE(IO,210)
         STOP
        ENDIF
       ENDIF
      ENDIF
C
      IF(ISOLN.EQ.1)THEN
       READ(IN,'(A80)') STRING
       ILINE=10
       READ(STRING,*,ERR=1000) RERRNR, RERRSUM, NMAX, NTMS, NS 
       IF(RERRNR.LT.0.0D0.OR.RERRSUM.LT.0.0D0)THEN
        WRITE(IO,108)
        STOP
       ENDIF
       IF(NMAX.LT.0.OR.NTMS.LT.0)THEN
        WRITE(IO,108)
        STOP
       ENDIF
       IF(NS.LT.0)THEN
        WRITE(IO,108)
        STOP
       ENDIF
       IF(NS.GT.20)THEN
        WRITE(IO,211)
        STOP
       ENDIF
      ENDIF
C
      IF(ISOLN.EQ.2)THEN
       READ(IN,'(A80)') STRING
       ILINE=10
       READ(STRING,*,ERR=1000) RERRNR, ERROR, NTMS, NNN, METHOD
       IF(RERRNR.LT.0.0D0.OR.ERROR.LT.0.0D0)THEN
        WRITE(IO,108)
        STOP
       ENDIF
       IF(NNN.LT.0.OR.NTMS.LT.0)THEN
        WRITE(IO,108)
        STOP
       ENDIF
       IF(METHOD.LT.0)THEN
        WRITE(IO,108)
        STOP
       ENDIF
      ENDIF
C
C---READ AND TEST PUMPED-WELL INFORMATION
      READ(IN,'(A80)') STRING
      ILINE=11
C---WTAQ Version 2: ADDED IPUMP
      READ(STRING,*,ERR=1000) IPWS, IPWD, IPUMP
C
      IF(IPWS.LT.0.OR.IPWS.GT.1)THEN
       WRITE(IO,109)
       STOP
      ENDIF
      IF(IPWD.LT.0.OR.IPWD.GT.1)THEN
       WRITE(IO,109)
       STOP
      ENDIF
C---WTAQ Version 2
      IF(IPUMP.LT.0.OR.IPUMP.GT.1)THEN
       WRITE(IO,109)
       STOP
      ENDIF
C
      READ(IN,'(A80)') STRING
      ILINE=12
      IF(IFORMAT.EQ.0)THEN
      READ(STRING,*,ERR=1000) RW, ZPD, ZPL, WD, SW
      ENDIF
      IF(IFORMAT.GE.1)THEN
      READ(STRING,*,ERR=1000) QQ, RW, RC, ZPD, ZPL, SW
      ENDIF
C
      IF(RW.LT.0.0D0.OR.RC.LT.0.0D0)THEN
       WRITE(IO,110)
       STOP
      ENDIF
      IF(ZPD.LT.0.0D0.OR.ZPL.LT.0.0D0)THEN
       WRITE(IO,110)
       STOP
      ENDIF
C---Version 1.1: add tests for partially-penetrating wells:
      IF(IPWS.EQ.0)THEN
       IF(ZPD.GE.ZPL)THEN
        WRITE(IO,146)
        STOP
       ENDIF
       IF(ZPD.GT.BB.OR.ZPL.GT.BB)THEN
        WRITE(IO,147)
        STOP
       ENDIF
      ENDIF
C
      IF(WD.LT.0.0D0.OR.SW.LT.0.0D0)THEN
       WRITE(IO,110)
       STOP
      ENDIF
      IF(QQ.LT.0.0D0)THEN
       WRITE(IO,110)
       STOP
      ENDIF
C
C12d-CALCULATE PUMPING-WELL DIMENSIONLESS PARAMETERS 
      RWD=RW/BB
      BETAW=XKD*(RWD*RWD)
      IF(IPWD.EQ.0)WD=0.0D0
      IF(IPWD.EQ.1)THEN
       IF(IFORMAT.GE.1)WD=((RC/RW)**2)/(2.0D0*SS*(ZPL-ZPD))
      ENDIF
      IF(IPWS.EQ.0)THEN
       XDD=ZPD/BB
       XLD=ZPL/BB
      ENDIF
      IF(IPWS.EQ.1)THEN
C---Version 1.1: add additional test:
        IF(ZPL.NE.BB)THEN
         WRITE(IO,148)
         STOP
        ENDIF
C
       BCALC=ZPL-ZPD
       IF(BCALC.NE.BB)THEN
        WRITE(IO,111)
        STOP
       ENDIF
      ENDIF
      IF(IPWS.EQ.1)THEN
       XDD=0.0D0
       XLD=1.0D0
      ENDIF
C
C---READ AND TEST PUMPED-WELL TIME-DRAWDOWN DATA
C
C---DEFAULT IS TO CALCULATE PUMPED WELL DRAWDOWN CURVES
      IRUN=1
C
      IF(IFORMAT.GE.1)THEN
       IF(ITS.EQ.1)THEN
        READ(IN,'(A80)') STRING
        ILINE=13
        READ(STRING,*,ERR=1000) NTSPW, IRUN
        IF(NTSPW.LT.0)THEN
         WRITE(IO,112)
         STOP
        ENDIF
        IF(IRUN.LT.0.OR.IRUN.GT.1)THEN
         WRITE(IO,112)
         STOP
        ENDIF
        IF(NTSPW.EQ.0)THEN
         IRUN=0
         GOTO 35
        ENDIF
        IF(IMEAS.EQ.0)THEN
         DO 20 ID=1,NTSPW
          READ(IN,'(A80)') STRING
          ILINE=14
          READ(STRING,*,ERR=1000) TIMEPW(ID)
  20     CONTINUE
        ENDIF
        IF(IMEAS.EQ.1)THEN
         DO 25 ID=1,NTSPW
          READ(IN,'(A80)') STRING
          ILINE=14
          READ(STRING,*,ERR=1000) TIMEPW(ID), XMEASPW(ID)
  25     CONTINUE
        ENDIF
        DO 30 ID=1,NTSPW
         IF(TIMEPW(ID).LT.0.0D0)THEN
          WRITE(IO,112)
          STOP
         ENDIF
  30    CONTINUE
       ENDIF
      ENDIF
C
C---READ AND TEST OBSERVATION-WELL INFORMATION
  35  READ(IN,'(A80)') STRING
      ILINE=15
      READ(STRING,*,ERR=1000) NOBWC
C
      IF(NOBWC.LT.0.OR.NOBWC.GT.(IMAXX1-1))THEN
       WRITE(IO,113)(IMAXX1-1)
       STOP
      ENDIF
C
C12e-WRITE INPUT DATA TO RESULT FILE
C
C---TITLE 
      WRITE(IO,114)TITLE
C
C---ANALYSIS TYPE AND AQUIFER TYPE
      IF(IFORMAT.EQ.0)FORMATTXT='TYPE-CURVE FORMAT'
      IF(IFORMAT.EQ.1)FORMATTXT='DIMENSIONAL FORMAT'
      IF(IAQ.EQ.0)AQTYPETXT='CONFINED AQUIFER'
      IF(IAQ.EQ.1)AQTYPETXT='WATER-TABLE AQUIFER'
      WRITE(IO,115)FORMATTXT,AQTYPETXT
C
C---HYDRAULIC PROPERTIES
      WRITE(IO,116)
C
      IF(IFORMAT.EQ.0)THEN
       WRITE(IO,117)BB
       WRITE(IO,118)XKD
       IF(IAQ.EQ.1)WRITE(IO,119)SIGMA
      ENDIF
C
      IF(IFORMAT.GE.1)THEN
       WRITE(IO,117)BB
       WRITE(IO,120)HKR
       WRITE(IO,121)HKZ
       WRITE(IO,118)XKD
       WRITE(IO,122)AT
       WRITE(IO,123)SS
       IF(IAQ.EQ.1)WRITE(IO,124)SY
       WRITE(IO,125)ASC
       IF(IAQ.EQ.1)WRITE(IO,119)SIGMA
      ENDIF
C
C---DRAINAGE PROPERTIES
      IF(IAQ.EQ.1)THEN
       IF(IDRA.EQ.0)WRITE(IO,126)IDRA
       IF(IDRA.EQ.1)THEN
        WRITE(IO,127)IDRA
        IF(IFORMAT.EQ.0)WRITE(IO,128)(GAMMA(I),I=1,NGAMMA)
        IF(IFORMAT.GT.0)THEN
         WRITE(IO,129)(ALPHA(I),I=1,NALPHA)
         WRITE(IO,128)(GAMMA(I),I=1,NGAMMA)
        ENDIF
       ENDIF
C---WTAQ VERSION 2
       IF(IDRA.EQ.2)THEN
        WRITE(IO,202)IDRA
        WRITE(IO,204)ACC,AKK,AMM,AXMM
        WRITE(IO,203)ADC,ADK,ADM,XMAXDM
        IF(AMM.GE.AXMM)THEN
         WRITE(IO,212)
         ELSE
         WRITE(IO,213)
        ENDIF
       ENDIF
      ENDIF
C
C---TIME-STEP AND PROGRAM-SOLUTION VARIABLES
      IF(IFORMAT.EQ.0)THEN
       WRITE(IO,130)
       WRITE(IO,131)TDLAST,NLC,NOX
      ENDIF
      IF(IFORMAT.GE.1)THEN
       IF(ITS.EQ.0)THEN
        WRITE(IO,132)
        WRITE(IO,131)TLAST,NLC,NOX
       ENDIF
       IF(ITS.EQ.1)THEN
        IF(IMEAS.EQ.0)WRITE(IO,133)
        IF(IMEAS.EQ.1)WRITE(IO,134)
       ENDIF
      ENDIF
C---WTAQ VERSION 2
      IF(ISOLN.EQ.1)THEN
       WRITE(IO,205)
       WRITE(IO,135)
       WRITE(IO,136)RERRNR,RERRSUM,NMAX,NTMS,NS
      ENDIF
      IF(ISOLN.EQ.2)THEN
       WRITE(IO,206)
       WRITE(IO,207)
       WRITE(IO,208)RERRNR,ERROR,NTMS,NNN,METHOD
      ENDIF
C
C---PUMPING-WELL INFORMATION
      WRITE(IO,137)
      IF(IPWD.EQ.0)WRITE(IO,138)IPWD
      IF(IPWD.EQ.1)WRITE(IO,139)IPWD
      IF(IPWS.EQ.0)WRITE(IO,140)IPWS
      IF(IPWS.EQ.1)WRITE(IO,141)IPWS
      IF(IFORMAT.GE.1)WRITE(IO,142)QQ
      WRITE(IO,143)
      WRITE(IO,144)RW,ZPD,ZPL,WD,SW
      WRITE(IO,145)BETAW
C
C---FORMAT STATEMENTS
  100 FORMAT(A70)
  101 FORMAT(A25)
  102 FORMAT(/,/,3X,'ANALYSIS FORMAT (LINE 2) IS INVALID.',
     2' PROGRAM STOPPED.')
  103 FORMAT(A11)
  104 FORMAT(/,/,3X,'AQUIFER TYPE IS INVALID. PROGRAM STOPPED.')
  105 FORMAT(/,3X,'A HYDRAULIC PROPERTY OF THE AQUIFER (INPUT LINE',
     2 ' 4) IS LESS THAN OR',/,3X,' EQUAL TO ZERO.',
     3 ' PROGRAM STOPPED.')
  106 FORMAT(/,3X,'A DRAINAGE VARIABLE OR PROPERTY (IDRA, NGAMMA,',
     2' NALPHA, GAMMA(I), OR',/,3X,'ALPHA(I)) IS NOT CORRECT.',
     3'  PROGRAM STOPPED.')
  107 FORMAT(/,3X,'A TIME-STEP VARIABLE (TDLAST, TLAST, NLC, NOX,',
     2'  ITS, OR IMEAS) IS LESS THAN',/,3X,' ZERO.  PROGRAM STOPPED.')
  108 FORMAT(/,3X,'A PROGRAM-SOLUTION VARIABLE (RERRNR AND SO FORTH)',
     2' IS LESS THAN ZERO',/,3X,'PROGRAM STOPPED.')
  109 FORMAT(/,3X,'EITHER IPWS, IPWD, OR IPUMP ARE NOT EQUAL TO',
     2' 0 or 1.  PROGRAM STOPPED.')
  110 FORMAT(/,3X,'A PUMPING-WELL VARIABLE (RW, RC, ZPD, ZPL, QQ, WD,',
     2' OR SW) IS LESS',/,3X,'THAN ZERO.  PROGRAM STOPPED.')
  111 FORMAT(/,2X,'ZPL-ZPD DOES NOT EQUAL INITIAL SATURATED THICKNESS',
     2' SPECIFIED FOR',/,2X,'AQUIFER. MODIFY ZPD, ZPL, OR BB.',
     3' PROGRAM STOPPED.')
  112 FORMAT(/,3X,'INFORMATION ON PUMPED-WELL TIME-DRAWDOWN DATA',
     2' (NTSPW, IRUN, OR TIMEPW(I))',/,3X,'IS NOT CORRECT.  PROGRAM',
     3' STOPPED.')
  113 FORMAT(/,2X,'SPECIFIED VALUE OF NOBWC IS LESS THAN ZERO OR ',
     2 'GREATER THAN',I3,'. NOBWC ',/,2X,'MUST BE LESS THAN OR EQUAL',
     3' TO (IMAXX1-1). IMAXX1 IS SPECIFIED IN THE',/,2X,'COMPUTER',
     4' CODE. REDUCE THE NUMBER OF OBSERVATION WELLS OR PIEZOMETERS',
     5 /,2X,'OR INCREASE THE VALUE OF IMAXX1 IN THE CODE.  PROGRAM',
     6 ' STOPPED.')
  114 FORMAT(3X,A70)
  115 FORMAT(/,/,3X,A27,5X,A20)
  116 FORMAT(/,/,/,15X,'*** AQUIFER HYDRAULIC PROPERTIES ***',/)
  117 FORMAT(2X,' SATURATED THICKNESS (BB): ',8X,D12.3,
     2' (units of length)')
  118 FORMAT(2X,' RATIO OF VERTICAL TO HORIZONTAL',/,3X,
     2' HYDRAULIC CONDUCTIVITY (XKD): ',3X,D12.3,' (dimensionless)')
  119 FORMAT(2X,' RATIO OF STORATIVITY TO',/,3X,
     2' SPECIFIC YIELD (SIGMA): ',9X,D12.3,' (dimensionless)')
  120 FORMAT(2X,' HORIZONTAL HYDRAULIC',/,3X,' CONDUCTIVITY (HKR): ',
     2 13X,D12.3,' (units of length per time)')
  121 FORMAT(2X,' VERTICAL HYDRAULIC',/,3x,' CONDUCTIVITY (HKZ): '
     2 13x,D12.3,' (units of length per time)')
  122 FORMAT(2X,' CALCULATED TRANSMISSIVITY: ',7X,D12.3,
     2' (units of length squared',/,51X,'per time)')
  123 FORMAT(2X,' SPECIFIC STORAGE (SS): ',11X,D12.3,
     2' (units of inverse length)')
  124 FORMAT(2X,' SPECIFIC YIELD (SY): ',13X,D12.3,
     2' (dimensionless)')
  125 FORMAT(2X,' CALCULATED STORATIVITY: ',10X,D12.3,
     2' (dimensionless)')
  126 FORMAT(2X,' DRAINAGE AT WATER TABLE (IDRA): ',3X,I3,
     2' (instantaneous)')
  127 FORMAT(2X,' DRAINAGE AT WATER TABLE (IDRA): ',3X,I3,
     2' (delayed)')
  128 FORMAT(2X,' DIMENSIONLESS DRAINAGE PARAMETER(S) (GAMMAS): ',/,
     2 3X,5(1PD11.2))
  129 FORMAT(2X,' DRAINAGE PARAMETER(S) (ALPHAS) (units of inverse',
     2' length):',/,3X,5(1PD11.2))
  130 FORMAT(/,/,/,16X,'*** PROGRAM SOLUTION VARIABLES ',
     2' ***',/,/,4X,'LARGEST VALUE',7X,'NUMBER OF',7X,'DRAWDOWN',
     3' CALCULATIONS',/,3X,'OF TIME (TDLAST)',3X,'LOG CYCLES',
     4' (NLC)',4X,'PER LOG CYCLE (NOX)',/,3X,'----------------',
     5'--------------------------------------------')
  131 FORMAT(7X,D9.3,11X,I2,18X,I2)
  132 FORMAT(/,/,/,16X,'*** PROGRAM SOLUTION VARIABLES ',
     2' ***',/,/,4X,'LARGEST VALUE',7X,'NUMBER OF',7X,'DRAWDOWN',
     3' CALCULATIONS',/,3X,'OF TIME (TLAST)',4X,'LOG CYCLES',
     4' (NLC)',4X,'PER LOG CYCLE (NOX)',/,3X,'----------------',
     5'--------------------------------------------')
  133 FORMAT(/,/,/,16X,'*** PROGRAM SOLUTION VARIABLES ',
     2' ***',/,/,3X,'USER-SPECIFIED TIMES; MEASURED DRAWDOWN',
     3' DATA NOT SPECIFIED')
  134 FORMAT(/,/,/,16X,'*** PROGRAM SOLUTION VARIABLES ',
     2' ***',/,/,3X,'USER-SPECIFIED TIMES; MEASURED DRAWDOWN',
     3' DATA SPECIFIED')
  135 FORMAT(5X,'RERRNR',6X,'RERRSUM',7X,'NMAX',7X,'NTMS',
     2 5X,'NS',/,3X,'------------------------------------------',
     3 '--------')
  136 FORMAT(4X,D9.3,3X,D9.3,4X,I5,6X,I5,6X,I2)
  137 FORMAT(/,/,/,3X,'*** PUMPED-WELL CHARACTERISTICS AND',
     2 ' CALCULATED DRAWDOWN ***',/)
  138 FORMAT(3X,'WELL-DIAMETER TYPE (IPWD):',11X,I1,
     2' (infinitesimal diameter)')
  139 FORMAT(3X,'WELL-DIAMETER TYPE (IPWD):',11X,I1,
     2 ' (finite diameter)')
  140 FORMAT(3X,'SCREENED INTERVAL (IPWS):',12X,I1,
     2' (partially penetrating)')
  141 FORMAT(3X,'SCREENED INTERVAL (IPWS):',12X,I1,
     2' (fully penetrating)')
  142 FORMAT(3X,'PUMPING RATE OF WELL (QQ):',9X,D11.3,
     2' (cubic length per time)')
  143 FORMAT(/,19X,'SCREENED INTERVAL',4X,'WELL BORE',3X,'WELL BORE',
     2/,4X,'WELL RADIUS',6X,'ZPD',6X,'ZPL',8X,'STORAGE',6X,'SKIN',
     3/,3X,'----------------------------------------------------------')
  144 FORMAT(5X,D8.2,5X,D8.2,2X,D8.2,4X,D8.2,4X,D8.2)
  145 FORMAT(/,/,3X,'BETAW = ',D12.3)
C---Version 1.1: add FORMAT statements:
  146 FORMAT(/,/,3X,'SPECIFIED TOP OF SCREENED INTERVAL OF PUMPED WELL',
     2' (ZPD) IS BELOW BOTTOM',/,12X,' OF SCREENED INTERVAL (ZPL).',
     3' PROGRAM STOPPED.')
  147 FORMAT(/,/,3X,'SPECIFIED VALUE OF TOP (ZPD) OR BOTTOM (ZPL)',
     2' OF SCREENED INTERVAL OF PUMPED WELL',/,5X,' IS GREATER',
     3' THAN SPECIFIED THICKNESS OF AQUIFER (BB).',
     4' PROGRAM STOPPED.')
  148 FORMAT(/,/,3X,'SPECIFIED VALUE OF BOTTOM OF SCREENED',
     2' INTERVAL OF PUMPED WELL (ZPL) DOES NOT EQUAL',/,5X,
     3' THICKNESS OF AQUIFER (BB). ZPD SHOULD EQUAL 0.0 AND ZPL',
     4' SHOULD EQUAL BB FOR A',/,5X,
     5' FULLY PENETRATING WELL. PROGRAM STOPPED.')
C
C---WTAQ VERSION 2
  200 FORMAT(/,3X,'AN UNSATURATED-ZONE CHARACTERISTIC IS SPECIFIED',
     2' AS LESS THAN 0. PROGRAM STOPPED.')
  201 FORMAT(/,3X,'ISOLN IS NOT EQUAL TO 1 OR 2. PROGRAM STOPPED.')
  202 FORMAT(2X,' DRAINAGE AT WATER TABLE (IDRA): ',3X,I3,
     2' (unsaturated-zone characteristics)')
  203 FORMAT(/,2X,' DIMENSIONLESS UNSATURATED-ZONE',
     2 ' PARAMETERS CALCULATED BY WTAQ: ',/,
     3 3X,' MOISTURE RETENTION EXPONENT (ADC): ',5X,D12.3,/,
     5 3X,' RELATIVE PERMEABILITY EXPONENT (ADK): ',2X,D12.3,/,     
     7 3X,' INITIAL UNSAT. ZONE THICKNESS (ADM): ',3X,D12.3,/,
     9 3X,' MAX. UNSAT. ZONE THICKNESS (XMAXDM): ',3X,D12.3)
  204 FORMAT(/,2X,' SPECIFIED UNSATURATED-ZONE',
     2 ' PARAMETERS: ',/,
     3 3X,' MOISTURE RETENTION EXPONENT (ACC): ',5X,D12.3,
     4 ' (units of inverse length)',/,
     5 3X,' RELATIVE PERMEABILITY EXPONENT (AKK): ',2X,D12.3,
     6 ' (units of inverse length)',/,     
     7 3X,' INITIAL UNSAT. ZONE THICKNESS (AMM): ',3X,D12.3,
     8 ' (units of length)',/,
     9 3X,' MAX. UNSAT. ZONE THICKNESS (AXMM):   ',3X,D12.3,
     & ' (units of length)')
  205 FORMAT(/,/,2X,' SOLUTION USING STEHFEST ALGORITHM: ')
  206 FORMAT(/,/,2X,' SOLUTION USING DE HOOG ALGORITHM: ')
  207 FORMAT(/,5X,'RERRNR',7X,'ERROR',9X,'NTMS',8X,'NNN',
     2 5X,'METHOD',/,3X,'------------------------------------------',
     3 '--------------')
  208 FORMAT(4X,D9.3,4X,D9.3,4X,I5,7X,I5,6X,I2)
  209 FORMAT(/,/,3X,'FOR CONFINED AQUIFERS, USE THE STEHFEST ALGORITHM',
     2 ' (ISOLN = 1). PROGRAM STOPPED.')
  210 FORMAT(/,/,3X,'FOR WATER-TABLE AQUIFERS WITH IDRA = 2, USE THE',
     2 ' DE HOOG ALGORITHM (ISOLN = 2). PROGRAM STOPPED.')
  211 FORMAT(/,/,3X,'NUMBER OF STEHFEST TERMS (NS) MUST BE LESS THAN',
     2 ' OR EQUAL TO 20. PROGRAM STOPPED.')
  212 FORMAT(/,2X,' INITIAL THICKNESS OF UNSATURATED ZONE IS GREATER',
     2 ' THAN THE MAXIMUM UNSATURATED',/,3X,'ZONE THICKNESS',
     3 ' SPECIFIED:',
     4 ' UNSATURATED ZONE THICKNESS ASSUMED INFINITE.')
  213 FORMAT(/,2X,' INITIAL THICKNESS OF UNSATURATED ZONE IS LESS',
     2 ' THAN THE MAXIMUM UNSATURATED',/,3X,'ZONE THICKNESS',
     3 ' SPECIFIED:',
     4 ' UNSATURATED ZONE THICKNESS ASSUMED TO BE FINITE.')
  214 FORMAT(/,/,3X,'ACC SPECIFIED TO BE GREATER THAN AKK. ACC MUST',
     2 ' BE LESS THAN OR EQUAL TO AKK.',/,3X, 'PROGRAM STOPPED.')
C
      RETURN
C---CALL RDERR FOR INPUT READ ERRORS
 1000 CALL RDERR(IO,STRING,ILINE)
      END
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE BANNER                   *
C      *                                                      *
C      *        VERSION 2.1 CURRENT AS OF 06/29/2012          *
C      *                                                      *
C       ******************************************************
C
C13-SUBROUTINE BANNER WRITES A PROGRAM BANNER TO RESULT FILE
      SUBROUTINE BANNER(IO)
      WRITE(IO,100)
 100  FORMAT(/,10X,
     2'      *****************************************************',/,
     3 10X,
     4'      *                                                   *',/,
     5 10X,
     6'      *         ****  U.S. GEOLOGICAL SURVEY  ****        *',/,
     7 10X,
     8'      *                                                   *',/,
     9 10X,
     &'      *           **** WTAQ: PROGRAM OUTPUT ****          *',/,
     1 10X,
     2'      *                                                   *',/,
     3 10X,
     4'      *     COMPUTER PROGRAM FOR CALCULATING DRAWDOWN     *',/,
     5 10X,
     6'      *                                                   *',/,
     7 10X,
     8'      *     IN A CONFINED OR WATER-TABLE AQUIFER WITH     *',/,
     9 10X,
     &'      *                                                   *',/,
     1 10X,
     2'      *       AXIAL-SYMMETRIC FLOW TO A FINITE- OR        *',/,
     3 10X,
     4'      *                                                   *',/,
     5 10X,
     6'      *        INFINITESIMAL-DIAMETER PUMPED WELL         *',/,
     7 10X,
     8'      *                                                   *',/,
     9 10X,
     &'      *              VERSION 2.1, JUNE 2012              *',/,
     1 10X,
     2'      *                                                   *',/,
     3 10X,
     4'      *****************************************************',
     5/,/)
C
      RETURN
      END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE RDERR                    *
C      *                                                      *
C      *        VERSION 2.0 CURRENT AS OF 11/10/2009          *
C      *                                                      *
C       ******************************************************
C
C14-SUBROUTINE RDERR WRITES AN ERROR MESSAGE TO THE RESULT FILE
C   AND STOPS PROGRAM EXECUTION
C
      SUBROUTINE RDERR(IO,STRING,ILINE)
C---SPECIFICATIONS
      CHARACTER STRING*80,VAR(19)*50
      DATA VAR/' TITLE',
     2         ' ANALYSIS FORMAT',
     3         ' AQUIFER TYPE',
     4         ' AQUIFER PROPERTIES',
     5         ' DRAINAGE TYPE AND NUMBER OF DRAINAGE PARAMETERS',
     6         ' DRAINAGE OR UNSATURATED-ZONE PARAMETERS',
     7         ' TDLAST NLC NOX',
     8         ' ITS IMEAS',
     9         ' TLAST NLC NOX',
     &         ' SOLUTION TYPE AND VARIABLES',
     1         ' IPWS IPWD',
     2         ' PUMPED-WELL CHARACTERISTICS',
     3         ' NTSPW IRUN',
     4         ' PUMPED-WELL MEASURED TIME-DRAWDOWN DATA',
     5         ' NOBWC',
     6         ' IOWS IDPR IRUN',
     7         ' OBSERVATION-WELL CHARACTERISTICS',
     8         ' NTSOB',
     9         ' OBSERVATION-WELL MEASURED TIME-DRAWDOWN DATA'/
      WRITE(IO,1)ILINE
      WRITE(IO,'(A80)')STRING
      WRITE(IO,2) VAR(ILINE)
      WRITE(IO,3)

   1  FORMAT(' Aborting due to error in line from input file:',
     2 2X,I5)
   2  FORMAT(' While attempting to read variables',A40)
   3  FORMAT(' Check for incorrect positions or data types...')
      STOP
      END
