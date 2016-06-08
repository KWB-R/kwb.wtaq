C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LINVST                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C15-SUBROUTINE LINVST CALCULATES COEFFICIENTS USED FOR THE
C   STEHFEST ALGORITHM
C
       SUBROUTINE LINVST(V,NS)
C---SPECIFICATIONS
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DIMENSION G(20),V(20),HS(20)
C
       G(1)=1.D0
       NH=NS/2
        DO 1 IS=2,NS
1      G(IS)=G(IS-1)*IS
       HS(1)=2.D0/G(NH-1)
        DO 3 IS=2,NH
       FI=IS
       IF(IS.EQ.NH) GO TO 2
       HS(IS)=FI**(NH)*G(2*IS)/(G(NH-IS)*G(IS)*G(IS-1))
       GO TO 3
2      HS(IS)=FI**(NH)*G(2*IS)/(G(IS)*G(IS-1))
3       CONTINUE
       SN=2*(NH-NH/2*2)-1
        DO 4 IS=1,NS
       V(IS)=0.D0
       K1=(IS+1)/2
       K2=IS
       IF(K2.GT.NH)K2=NH
        DO 5 KS=K1,K2
       IF(2*KS-IS.EQ.0) GO TO 6
       IF(IS.EQ.KS)GO TO 7
       V(IS)=V(IS)+HS(KS)/(G(IS-KS)*G(2*KS-IS))
       GO TO 5
6      V(IS)=V(IS)+HS(KS)/(G(IS-KS))
       GO TO 5
7      V(IS)=V(IS)+HS(KS)/G(2*KS-IS)
5       CONTINUE
       V(IS)=SN*V(IS)
       SN=-SN
4       CONTINUE
       RETURN
       END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LTST1                    *
C      *                                                      *
C      *        VERSION 2.0 CURRENT AS OF 01/22/2010          *
C      *                                                      *
C       ******************************************************
C
C16-SUBROUTINE LTST1 CALCULATES THE LAPLACE TRANSFORM SOLUTION FOR
C   DRAWDOWN FOR FLOW TO A FULLY PENETRATING WELL OF INFINITESIMAL
C   DIAMETER IN A CONFINED AQUIFER (THEIS SOLUTION).
C
       SUBROUTINE LTST1(TD,HDT)
C---SPECIFICATIONS
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C---COMMON STATEMENTS
C---WTAQ VERSION 2
      COMMON /PAR2/ NGAMMA,IDRA,NS,KK,NMAX,NTMS,ISOLN,NNN,METHOD
C
      COMMON /PAR9/ V(20),XLN2,EXPMAX
      COMMON /PAR10/ RD,ZD,ZD1,ZD2
C
       XP=0.D0
      DO 1 I=1,NS
       PP=XLN2*I/TD
C
       CA=RD*DSQRT(PP)
       IF(CA.GT.EXPMAX) CA=EXPMAX
       RE0=BESSK0(CA)
       PDL=RE0/PP
1     XP=XP+V(I)*PDL
       HDT=2.D0*XP*XLN2/TD
C
C---RETURN TO MAIN PROGRAM
       RETURN
       END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LTST2                    *
C      *                                                      *
C      *        VERSION 2.0 CURRENT AS OF 01/22/2010          *
C      *                                                      *
C       ******************************************************
C
C17-SUBROUTINE LTST2 CALCULATES THE LAPLACE TRANSFORM SOLUTION FOR
C   DRAWDOWN FOR FLOW TO A FINITE DIAMETER, PARTIALLY PENETRATING
C   WELL IN A CONFINED AQUIFER (MODIFIED SOLUTION OF DOUGHERTY AND
C   BABU, 1984). DELAYED DRAWDOWN RESPONSE AT OBSERVATION WELLS
C   IS INCLUDED.
C
       SUBROUTINE LTST2(TD,HD)
C---SPECIFICATIONS
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C---COMMON STATEMENTS
      COMMON /IOUNIT/ IN,IO,IP,ILOG
C---WTAQ VERSION 2
      COMMON /PAR1/ IPWD,IRUN,IPWS,NOBWC,IOWS,IDPR,IPUMP
      COMMON /PAR2/ NGAMMA,IDRA,NS,KK,NMAX,NTMS,ISOLN,NNN,METHOD
C
      COMMON /PAR6/ BETAW,SIGMA,GAMMA
      COMMON /PAR7/ RERRNR,RERRSUM,ERROR,TDLAST,TLAST
      COMMON /PAR8/ R,ZP,Z1,Z2,WDP
      COMMON /PAR9/ V(20),XLN2,EXPMAX
      COMMON /PAR10/ RD,ZD,ZD1,ZD2
      COMMON /PAR11/ XLD,XDD,WD,SW
C
       HD=0.D0
       IF(IRUN.EQ.0.AND.KK.EQ.1) RETURN
C
       PI=3.141592653589793D0
C
       IF(IPWS.EQ.1) THEN
        XDD=0.D0
        XLD=1.D0
       ENDIF
C
       XP=0.0D0
C
      DO 1 I=1,NS
       PP=XLN2*I/TD
       Q0=DSQRT(PP)
       Q0RD=Q0*RD
       IF(Q0.GT.EXPMAX) Q0=EXPMAX
       IF(Q0RD.GT.EXPMAX) Q0RD=EXPMAX
       RE0=BESSK0(Q0)
       RE1=BESSK1(Q0)
       RE0X=BESSK0(Q0RD)
       A0=RE0*(XLD-XDD)/(Q0*RE1)
       E0=RE0X*(XLD-XDD)/(Q0*RE1)
       A=0.D0
       E=0.D0
       IF(IPWS.EQ.1) GOTO 30
       IF(IOWS.EQ.1) GOTO 30
       SUMA=0.D0
       SUME=0.D0
C
       NNN=0
C
10     NNN=NNN+1
       IF(NNN.GE.NMAX) GOTO 40
       SUMTA=SUMA
       SUMTE=SUME
       XNPI=NNN*PI
       QN=DSQRT(BETAW*XNPI*XNPI+PP)
       IF(QN.GT.EXPMAX) QN=EXPMAX
       DB=DSIN(XNPI*(1.0D0-XDD))
       DA=DSIN(XNPI*(1.0D0-XLD))
       IF(IPWS.EQ.1) DA=0.D0
       SINES=DB-DA
       RE0=BESSK0(QN)
       RE1=BESSK1(QN)
       XNUM=RE0*SINES*SINES/(XNPI*(XLD-XDD))
       XDEN=0.5D0*QN*RE1*XNPI
       A=XNUM/XDEN
       SUMA=SUMTA+A
C
       IF(KK.GT.1)THEN
        QNRD=QN*RD
        IF(QNRD.GT.EXPMAX) QNRD=EXPMAX
        RE0X=BESSK0(QNRD)
        IF(IOWS.EQ.0) 
     1   XNUM=RE0X*SINES*(DSIN(XNPI*ZD2)
     2   -DSIN(XNPI*ZD1))/(XNPI*(ZD2-ZD1))
        IF(IOWS.EQ.2) 
     1   XNUM=RE0X*SINES*DCOS(XNPI*ZD)
        E=XNUM/XDEN
        SUME=SUMTE+E
       ENDIF
C
       IF(IPWS.EQ.0.AND.NNN.LT.25) GOTO 10
       ERRA=DABS(SUMTA-SUMA)
       IF(KK.EQ.1)THEN
        IF(ERRA.LT.RERRSUM*SUMA) GOTO 40
       ENDIF
       IF(KK.GT.1)THEN
        ERRE=DABS(SUMTE-SUME)
        IF(ERRA.LT.RERRSUM*SUMA.AND.ERRE.LT.RERRSUM*SUME) GOTO 40
       ENDIF
C
       GOTO 10
C
40     CONTINUE
C
       A=SUMA
30     DENOM=(1.D0+WD*PP*(A0+A+SW))
       IF(KK.EQ.1) PDL=(A0+A+SW)/(PP*DENOM)
       IF(KK.GT.1) THEN
         E=SUME
         IF(IDPR.EQ.0) PDL=(E0+E)/(PP*DENOM)
         IF(IDPR.EQ.1) THEN
            SLUGF=1.D0/(1.D0+WDP*PP)
            PDL=SLUGF*(E0+E)/(PP*DENOM)
         ENDIF
       ENDIF
C
      XP=XP+V(I)*PDL
C
1     CONTINUE
C
       HD=2.D0*XP*XLN2/(TD*(XLD-XDD))
C
       IF(NNN.GE.NMAX) WRITE(IO,100)
C
C---FORMAT STATEMENTS
 100   FORMAT('PROGRAM CONTINUES TO NEXT TIME STEP BECAUSE NNN',
     2' EXCEEDS NMAX.')
C
C---RETURN TO MAIN PROGRAM
       RETURN
       END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LTST3                    *
C      *                                                      *
C      *        VERSION 1.0 CURRENT AS OF 01/22/2010          *
C      *                                                      *
C       ******************************************************
C
C18-SUBROUTINE LTST3 CALCULATES THE LAPLACE TRANSFORM SOLUTION FOR
C   DRAWDOWN FOR FLOW TO A FINITE DIAMETER, PARTIALLY PENETRATING
C   WELL IN A WATER-TABLE AQUIFER WITH DELAYED DRAWDOWN RESPONSE AT
C   OBSERVATION WELLS.
C
       SUBROUTINE LTST3(TD,HD)
C---SPECIFICATIONS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GAMMA(5)
C---COMMON STATEMENTS
      COMMON /IOUNIT/ IN,IO,IP,ILOG
C---WTAQ VERSION 2
      COMMON /PAR1/ IPWD,IRUN,IPWS,NOBWC,IOWS,IDPR,IPUMP
      COMMON /PAR2/ NGAMMA,IDRA,NS,KK,NMAX,NTMS,ISOLN,NNN,METHOD
C
      COMMON /PAR6/ BETAW,SIGMA,GAMMA
      COMMON /PAR7/ RERRNR,RERRSUM,ERROR,TDLAST,TLAST
      COMMON /PAR8/ R,ZP,Z1,Z2,WDP
      COMMON /PAR9/ V(20),XLN2,EXPMAX
      COMMON /PAR10/ RD,ZD,ZD1,ZD2
      COMMON /PAR11/ XLD,XDD,WD,SW
      COMMON /PAR13/ ADC,ADK,ADM,BETA,XMAXDM
C
       SLUGF=0.0D0
       HD=0.D0
       IF(IRUN.EQ.0.AND.KK.EQ.1) RETURN
C
C
C18a-EMPIRICAL ESTIMATE OF NECESSARY NUMBER OF XN TERMS NEEDED FOR
C    THE FINITE SUMS. (SMALL VALUES OF BETA AND TD REQUIRE MORE
C    TERMS THAN DO LARGE VALUES.)
       XN_MAX=NTMS*2.D0**((-DLOG10(BETA)-2.D0))
       NX=IDNINT(XN_MAX)
       IF(NX.LT.4) NX=4
C
       PI=3.141592653589793D0
C
       IF(IPWS.EQ.1) THEN
        XDD=0.D0
        XLD=1.D0
       ENDIF
       IF(IOWS.EQ.1) THEN
        ZD1=0.D0
        ZD2=1.D0
       ENDIF
C
       XP=0.0D0
C
      DO 1 I=1,NS
       PP=XLN2*I/TD
       IF(IDRA.EQ.1)THEN
        RHS=0.0D0
        DO 5 J=1,NGAMMA
         RHS1=1.0D0/(SIGMA*BETAW+PP/GAMMA(J))
         RHS=RHS+RHS1
  5     CONTINUE
        XNGAMMA=NGAMMA
        RHS=PP*RHS/XNGAMMA
       ELSE
        RHS=PP/(SIGMA*BETAW)
       ENDIF
C
       SUMA=0.D0
       SUME=0.D0
       NNN=0
10     NNN=NNN+1
C
       IF(NNN.EQ.NX) GOTO 40
C
C     ASSIGN A STARTING POINT FOR EPS
       IF(NNN.GT.1) GOTO 15
       IF(RHS.LT.1.D0) EPS=DSQRT(RHS)
       IF(RHS.GE.1.D0) EPS=DATAN(RHS)
15     IF(NNN.GT.1) EPS=EPS0+PI
C
       SUMTA=SUMA
       SUMTE=SUME
C
C---CALCULATE EPSILON FOR CURRENT VALUE OF PP USING NEWTON-RAPHSON
C    TECHNIQUE
       N=0
30     EPS0=EPS
       N=N+1
       IF(N.GT.100)WRITE(IO,100)
       IF(N.GT.100)STOP
       A1=DSIN(EPS0)
       A2=DCOS(EPS0)
       F=EPS0*A1-RHS*A2
       FP=EPS0*A2+A1+RHS*A1
       EPS=EPS0-F/FP
       IF(DABS(EPS-EPS0).GT.RERRNR*EPS) GOTO 30
C
       QN=DSQRT(BETAW*EPS*EPS+PP)
       IF(QN.GT.EXPMAX) QN=EXPMAX
       DB=DSIN(EPS*(1.0D0-XDD))
       DA=DSIN(EPS*(1.0D0-XLD))
       IF(IPWS.EQ.1) DA=0.D0
       SINES=DB-DA
C
       RE0=BESSK0(QN)
       RE1=BESSK1(QN)
       XNUM=RE0*SINES*SINES/(EPS*(XLD-XDD))
       XDEN=0.5D0*QN*RE1*(EPS+0.5D0*DSIN(2.D0*EPS))
       A=XNUM/XDEN
       SUMA=SUMTA+A
C
       IF(KK.GT.1)THEN
        QNRD=QN*RD
        IF(QNRD.GT.EXPMAX) QNRD=EXPMAX
        RE0X=BESSK0(QNRD)
C
C---IF IOWS.EQ.0, THEN PARTIALLY PENETRATING OBSERVATION WELL
        IF(IOWS.EQ.0) 
     1   XNUM=RE0X*SINES*(DSIN(EPS*ZD2)
     2   -DSIN(EPS*ZD1))/(EPS*(ZD2-ZD1))
C
C---IF IOWS.EQ.1, THEN FULLY PENETRATING OBSERVATION WELL
        IF(IOWS.EQ.1) 
     1   XNUM=RE0X*SINES*DSIN(EPS)/EPS
C
C---IF IOWS.EQ.2, THEN OBSERVATION PIEZOMETER
        IF(IOWS.EQ.2) 
     1   XNUM=RE0X*SINES*DCOS(EPS*ZD)
        E=XNUM/XDEN
        SUME=SUMTE+E
       ENDIF
C
       GOTO 10
C
 40    CONTINUE
C
       A=SUMA
       DENOM=(1.D0+WD*PP*(A+SW))
       IF(KK.EQ.1) PDL=(A+SW)/(PP*DENOM)
       IF(KK.GT.1) THEN
         E=SUME
         IF(IDPR.EQ.0) PDL=E/(PP*DENOM)
         IF(IDPR.EQ.1) THEN
            SLUGF=1.D0/(1.D0+WDP*PP)
            PDL=SLUGF*E/(PP*DENOM)
         ENDIF
       ENDIF
C
      XP=XP+V(I)*PDL
C
1     CONTINUE
C
      HD=2.D0*XP*XLN2/(TD*(XLD-XDD))
C
C---FORMAT STATEMENTS
  100 FORMAT(' PROGRAM STOPPED BECAUSE NEWTON-RAPHSON ITERATIONS',
     2' EXCEED 100')
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSK0                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C19-FUNCTION BESSK0 CALCULATES THE ZERO-ORDER MODIFIED BESSEL FUNCTION
C    OF THE SECOND KIND. SOURCE: PRESS AND OTHERS (1992).
      DOUBLE PRECISION FUNCTION BESSK0(X)
C---SPECIFICATIONS
      DOUBLE PRECISION X,Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,BESSI0
      DATA P1,P2,P3,P4,P5,P6,P7/-0.57721566D0,0.42278420D0,0.23069756D0,
     *    0.3488590D-1,0.262698D-2,0.10750D-3,0.74D-5/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,-0.7832358D-1,0.2189568D-1,
     *    -0.1062446D-1,0.587872D-2,-0.251540D-2,0.53208D-3/
C
      IF (X.LE.2.D0) THEN
        Y=X*X/4.D0
        BESSK0=(-DLOG(X/2.D0)*BESSI0(X))+(P1+Y*(P2+Y*(P3+
     *        Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        Y=(2.D0/X)
        BESSK0=(DEXP(-X)/DSQRT(X))*(Q1+Y*(Q2+Y*(Q3+
     *        Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSI0                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C20-FUNCTION BESSI0 CALCULATES THE ZERO-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE FIRST KIND. SOURCE: PRESS AND OTHERS (1992).
      DOUBLE PRECISION FUNCTION BESSI0(X)
C---SPECIFICATIONS
      DOUBLE PRECISION X,Y,AX,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,3.5156229D0,3.0899424D0,1.2067492D
     *0,
     *    0.2659732D0,0.360768D-1,0.45813D-2/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,0.1328592D-1,
     *    0.225319D-2,-0.157565D-2,0.916281D-2,-0.2057706D-1,
     *    0.2635537D-1,-0.1647633D-1,0.392377D-2/
C
      IF (DABS(X).LT.3.75D0) THEN
        Y=(X/3.75D0)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      ELSE
        AX=DABS(X)
        Y=3.75D0/AX
        BESSI0=(DEXP(AX)/DSQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4
     *      +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSK1                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C21-FUNCTION BESSK1 CALCULATES THE FIRST-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE SECOND KIND. SOURCE: PRESS AND OTHERS (1992).
      DOUBLE PRECISION FUNCTION BESSK1(X)
C---SPECIFICATIONS
      DOUBLE PRECISION X,Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,BESSI1
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0,
     *    -0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1,
     *    0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/
C
      IF (X.LE.2.0) THEN
        Y=X*X/4.0
        BESSK1=(LOG(X/2.0)*BESSI1(X))+(1.0/X)*(P1+Y*(P2+
     *      Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        Y=2.0/X
        BESSK1=(EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+
     *      Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C
C       ******************************************************
C      *                                                      *
C      *                    FUNCTION BESSI1                   *
C      *                                                      *
C      *          VERSION 1.0 CURRENT AS OF 10/01/99          *
C      *                                                      *
C       ******************************************************
C
C22-FUNCTION BESSI1 CALCULATES THE FIRST-ORDER MODIFIED BESSEL FUNCTION 
C    OF THE FIRST KIND. SOURCE: PRESS AND OTHERS (1992).
      DOUBLE PRECISION FUNCTION BESSI1(X)
C---SPECIFICATIONS
      DOUBLE PRECISION X,Y,AX,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0,
     *    0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1,
     *    -0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1,
     *    -0.2895312D-1,0.1787654D-1,-0.420059D-2/
C
      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI1=X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI1=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+
     *      Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
C
C---RETURN TO MAIN PROGRAM
      RETURN
      END
C
C
C       ******************************************************
C      *                                                      *
C      *                   SUBROUTINE FUN                     *
C      *                                                      *
C      *         VERSION 2.0 CURRENT AS OF 11/13/2009         *
C      *                                                      *
C       ******************************************************
C
       SUBROUTINE FUN(CP,CF)
C
C      THIS SUBPROGRAM EVALUATES THE LAPLACE TRANSFORM SOLUTION FOR
C      FLOW TO A FINITE-DIAMETER, PARTIALLY PENETRATING WELL IN A
C      WATER-TABLE AQUIFER USING THE DEHOOG ALGORITHM.
C
C      WRITTEN BY ALLEN F. MOENCH
C
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C---WTAQ VERSION 2
       DIMENSION GAMMA(5)
C
       COMPLEX*16 CF,CP,CZERO,CUNITY
       COMPLEX*16 CRHS,CRHS1
       COMPLEX*16 CSUMA,CSUME,CXN,CEPS
       COMPLEX*16 CEPS0,CSUMTA,CSUMTE,CA1,CA2,CFFP,CFF,CQN
       COMPLEX*16 CQNRD,CDB,CDA,CSINES,CK0,CK1,CK0RD,CK1RD
       COMPLEX*16 CXNUM,CXDEN,CA,CE,CDENOM,CPDL,CSLUGF
C---WTAQ VERSION 2
C---COMMON STATEMENTS
      COMMON /PAR1/ IPWD,IRUN,IPWS,NOBWC,IOWS,IDPR,IPUMP
      COMMON /PAR2/ NGAMMA,IDRA,NS,KK,NMAX,NTMS,ISOLN,NNN,METHOD
      COMMON /PAR6/ BETAW,SIGMA,GAMMA
      COMMON /PAR7/ RERRNR,RERRSUM,ERROR,TDLAST,TLAST
      COMMON /PAR8/ R,ZP,Z1,Z2,WDP
      COMMON /PAR10/ RD,ZD,ZD1,ZD2
      COMMON /PAR11/ XLD,XDD,WD,SW
      COMMON /PAR13/ ADC,ADK,ADM,BETA,XMAXDM
      COMMON /IOUNIT/ IN,IO,IP,ILOG
C
c---WTAQ VERSION 2: CONVERT VARIABLES FROM WTAQ4 TO WTAQ
       NALPHA=NGAMMA
       WDPRIME=WDP
C
       CZERO=DCMPLX(0.D0,0.D0)
       CUNITY=DCMPLX(1.D0,0.D0)
C
       CSLUGF=CZERO
C
C     ESTIMATE OF THE NUMBER OF XN TERMS NEEDED FOR THE
C     FINITE SUM. (SMALL VALUES OF BETA AND TD REQUIRE MORE TERMS THAN
C     LARGE VALUES.)
C
       XN_MAX=NTMS*2.D0**((-DLOG10(BETA)-2.D0))
       NN=IDNINT(XN_MAX)
       IF(NN.LT.4) NN=4
C
       PI=3.141592653589793D0
       ZDA=1.D0-XLD
       ZDB=1.D0-XDD
C
       IF(IPWS.EQ.1) THEN
        ZDA=0.D0
        ZDB=1.D0
       ENDIF
       IF(IOWS.EQ.1) THEN
        ZD1=0.D0
        ZD2=1.D0
       ENDIF
C
C     DETERMINATION OF THE VALUE OF CRHS, THE RIGHT-HAND-SIDE OF (24)
C     IN MOENCH (1997) OR (42) IN MATHIAS AND BUTLER (2006):
        IF(IDRA.EQ.0) CRHS=CP/(SIGMA*BETAW*CUNITY)
        IF(IDRA.EQ.1) THEN
         CRHS=CZERO
         DO 1 J=1,NALPHA
         CRHS1=1.D0/(SIGMA*BETAW*CUNITY+CP/(GAMMA(J)*CUNITY))
         CRHS=CRHS+CRHS1
 1       CONTINUE
         XNALPHA=NALPHA
         CRHS=CP*CRHS/(XNALPHA*CUNITY)
        ENDIF
CCC
	 CP=CP*(RD*RD)/SIGMA
       IF(IDRA.EQ.2) CALL MATHIAS(CP,CRHS)
	 CP=CP*SIGMA/(RD*RD)
CCC
       CSUMA=CZERO
       CSUME=CZERO
       XN=0.D0
 10    XN=XN+1.D0
C
C     ASSIGN A STARTING POINT FOR EPS
       IF(IDNINT(XN).GT.1) GOTO 15
       IF(CDABS(CRHS).LT.1.D0) CEPS=CDSQRT(CRHS)
	 IF(CDABS(CRHS).GE.1.D0) CEPS=PI*0.25D0*CUNITY
 15    IF(IDNINT(XN).GT.1) CEPS=CEPS0+PI*CUNITY
C
       CSUMTA=CSUMA
       CSUMTE=CSUME
       N=0
 30    CEPS0=CEPS
       N=N+1
       IF(N.GT.100)WRITE(IO,100)
       IF(N.GT.100)STOP
       CA1=CDSIN(CEPS0)
       CA2=CDCOS(CEPS0)
       CFF=CEPS0*CA1-CRHS*CA2
       CFFP=CEPS0*CA2+CA1+CRHS*CA1
       CEPS=CEPS0-CFF/CFFP
       IF(CDABS(CEPS-CEPS0).GT.RERRNR*CDABS(CEPS)) GOTO 30
C
       CQN=CDSQRT(BETAW*CEPS*CEPS+CP)
       CQNRD=CQN*RD
       CDB=CDSIN(CEPS*ZDB)
       CDA=CDSIN(CEPS*ZDA)
       IF(IPWS.EQ.1) CDA=CZERO
       CSINES=CDB-CDA
C
       CALL BESKZ1(CQN,CK0,CK1)
       CALL BESKZ1(CQNRD,CK0RD,CK1RD)
       CXNUM=CK0*CSINES*CSINES/(CEPS*(XLD-XDD))
       CXDEN=0.5D0*CQN*CK1*(CEPS+0.5D0*CDSIN(2.D0*CEPS))
       CA=CXNUM/CXDEN
       CSUMA=CSUMTA+CA
C
       IF(KK.EQ.1) THEN
         IF(IDNINT(XN).EQ.NN) GOTO 40
         GOTO 10
       ENDIF
C
       IF(IOWS.EQ.0) 
     1 CXNUM=CK0RD*CSINES*(CDSIN(CEPS*ZD2)
     2 -CDSIN(CEPS*ZD1))/(CEPS*(ZD2-ZD1))
       IF(IOWS.EQ.1) 
     1 CXNUM=CK0RD*CSINES*CDSIN(CEPS)/CEPS
       IF(IOWS.EQ.2) 
     1 CXNUM=CK0RD*CSINES*CDCOS(CEPS*ZD)
       CE=CXNUM/CXDEN
       CSUME=CSUMTE+CE
C
          IF(IDNINT(XN).EQ.NN) GOTO 40
          GOTO 10
 40    CONTINUE
       CA=CSUMA
       CDENOM=(CUNITY+WD*CP*(CA+SW*CUNITY))
       IF(KK.EQ.1) CPDL=(CA+SW*CUNITY)/(CP*CDENOM)
       IF(KK.GT.1) THEN
         CE=CSUME
         IF(IDPR.EQ.0) CPDL=CE/(CP*CDENOM)
         IF(IDPR.EQ.1) THEN
            CSLUGF=CUNITY/(CUNITY+WDPRIME*CP)
            CPDL=CSLUGF*CE/(CP*CDENOM)
         ENDIF
       ENDIF
       CF=2.D0*CPDL/((ZDB-ZDA)*CUNITY)
C
C---FORMAT STATEMENTS
  100 FORMAT(' PROGRAM STOPPED BECAUSE NEWTON-RAPHSON ITERATIONS',
     2' EXCEED 100')
C
C---RETURN TO MAIN PROGRAM
       RETURN
       END
C
C
C       ******************************************************
C      *                                                      *
C      *                 SUBROUTINE MATHIAS                   *
C      *                                                      *
C      *         VERSION 2.0 CURRENT AS OF 01/20/2011         *
C      *                                                      *
C       ******************************************************
C
       SUBROUTINE MATHIAS(CP,CRHS)
C   
C   THIS VERSION OF THE SUBROUTINE MATHIAS INCLUDES THE
C   UNSATURATED ZONE THICKNESS. ADK MUST BE GREATER THAN
C   OR EQUAL TO ADC.
C
C   WRITTEN BY ALLEN F. MOENCH
C
CC
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       COMPLEX*16 CP,CRHS,CARG0,CRATIO
       COMPLEX*16 CZERO,CUNITY,CIMAG
       COMPLEX*16 C1,C11,C2,C22,C4,C5,CALPHA,CNUM,CDEN
       COMPLEX*16 CEQ42,CFAC,CHI0,CHI0I,CGAM,CG
       COMPLEX*16 C3,C33,C6,CHIM,CHIMI,CARGM
 	 DIMENSION CWRKR(1),CWRKI(1),CYR(1),CYI(1)
C
       COMMON /PAR13/ ADC,ADK,ADM,BETA,XMAXDM
       COMMON /PAR14/ TDPASS,F1
       COMMON /PAR15/ IFORMATPASS
       COMMON /IOUNIT/ IN,IO,IP,ILOG
C
       AXDM=XMAXDM
CC
       CZERO=DCMPLX(0.D0,0.D0)
       CUNITY=DCMPLX(1.D0,0.D0)
	 CIMAG=DCMPLX(0.D0,1.D0)
	 EXPMAX=700.D0
       PI=3.141592653589793D0
C
C     THIS WILL ALWAYS BE NEGATIVE, BECAUSE PROGRAM CHECKS
C      FOR ADK>=ACC
       DIF=ADC-ADK
CC
CC    IF ADC = ADK, AS ASSUMED BY KROSZYNSKI & DAGAN (1975):
    	  IF(DABS(DIF).LT.0.01) THEN
C      EQUATIONS 49 AND 52
    	      CGAM=CDSQRT(CUNITY+4.D0*CP/(ADC*BETA))
    	      CG=((CUNITY-CGAM)/(CUNITY+CGAM))*CDEXP(-ADC*CGAM*ADM)
CC    FOR INFINITELY THICK UNSATURATED ZONE, AS ASSUMED BY K&D (1975):
		IF(ADM.GE.AXDM) THEN	
C      EQUATION 54
    	      CRHS=(ADC/2.D0)*(CGAM-CUNITY)
CC    FOR A THIN UNSATURATED ZONE (SPECIAL CASE MATHIAS AND BUTLER):
    	      ELSE
C      EQUATION 53
    	      CRHS=(ADC/2.D0)*(CGAM*((CUNITY+CG)/(CUNITY-CG))-CUNITY)
    	      ENDIF
    	  RETURN
    	  ENDIF
CC
CC     EQUATIONS 39 AND 40
       XNU=ADK/DIF
       XMU=ADC/DIF
	 IU=0
CC
CC     FOR NEGATIVE ORDERS APPLY THE FORMULAE AT END OF SUBROUTINE
CC     (THE ORDER WILL LIKELY BE NEGATIVE IN REAL SYSTEMS)
CC
      IF(DIF.LT.0.D0) THEN
	 IU=1
       XNU=-XNU
       XMU=-XMU
	ENDIF
CC
C     THIS IS EQUATION 38 FOR Z=0 (AT WATER TABLE)
       CARG0=ADC*CP/(BETA*DIF*DIF)
       CHI0=2.D0*CDSQRT(CARG0)
C     THIS IS ARUGUMENT FOR EQUATION 37 FOR Z=0
	 CHI0I=CIMAG*CHI0
       XREAL0=DREAL(CHI0I)
       XIMAG0=DIMAG(CHI0I)
CC
CC    FOR A FINITE-THICKNESS UNSATURATED ZONE:
CC
        ABSZ=100.D0
CC    IF ADM >= AXDM, THEN AN INFINITELY THICK UNSATURATED
CC     ZONE HAS BEEN SPECIFIED...SKIP THIS PART
        IF(ADM.GE.AXDM) GO TO 1
C      THIS IS EQUATION 38 FOR Z=M (THICKNESS OF UNSAT ZONE)
	  A=-DIF*ADM
	  IF(A.GT.EXPMAX) A=EXPMAX
	  CARGM=ADC*CP*DEXP(A)/(BETA*DIF*DIF)
C      THIS IS ARGUMENT FOR EQUATION 37 FOR Z=M
	  CHIM=2.D0*CDSQRT(CARGM)
	  CHIMI=CIMAG*CHIM
        ABSZ=CDABS(CHIM)
	  XREALM=DREAL(CHIMI)
	  XIMAGM=DIMAG(CHIMI)
1       CONTINUE
CC
CC    COMPUTE THE BESSEL FUNCTIONS J AND Y:
CC
C     Ju(0)
       CALL ZBESJ(XREAL0,XIMAG0,XMU,2,1,CYR,CYI,NZ,IERR)
             IF(IERR.NE.0) WRITE(ILOG,*)'IERR1=',IERR,'  NZ1=',NZ
       C1=DCMPLX(CYR(1),CYI(1))
       C11=C1
C     Jv(0)
       CALL ZBESJ(XREAL0,XIMAG0,XNU,2,1,CYR,CYI,NZ,IERR)
             IF(IERR.NE.0) WRITE(ILOG,*)'IERR2=',IERR,'  NZ2=',NZ
       C2=DCMPLX(CYR(1),CYI(1))
       C22=C2
c     Ju(m):Only calculate if unsaturated zone is not infinitely
c       thick or if (finite-thick) unsaturated zone is relatively
c       thin
	    IF(ABSZ.LT.20.D0) THEN
       CALL ZBESJ(XREALM,XIMAGM,XMU,2,1,CYR,CYI,NZ,IERR)
             IF(IERR.NE.0) WRITE(ILOG,*)'IERR3=',IERR,'  NZ1=',NZ
	 C3=DCMPLX(CYR(1),CYI(1))
       C33=C3
	    ENDIF
c     Yu(0)
	 CALL ZBESY(XREAL0,XIMAG0,XMU,2,1,CYR,CYI,NZ,CWRKR,CWRKI,IERR)
             IF(IERR.NE.0) WRITE(ILOG,*)'IERR4=',IERR,'  NZ4=',NZ
       C4=DCMPLX(CYR(1),CYI(1))
c     Yv(0)
       CALL ZBESY(XREAL0,XIMAG0,XNU,2,1,CYR,CYI,NZ,CWRKR,CWRKI,IERR)
             IF(IERR.NE.0) WRITE(ILOG,*)'IERR5=',IERR,'  NZ5=',NZ
       C5=DCMPLX(CYR(1),CYI(1))
c     Yu(m):Only calculate if unsaturated zone is not infinitely
c      thick or if (finite-thick) unsaturated zone is relatively
c      thin
	    IF(ABSZ.LT.20.D0) THEN
       CALL ZBESY(XREALM,XIMAGM,XMU,2,1,CYR,CYI,NZ,CWRKR,CWRKI,IERR)
             IF(IERR.NE.0) WRITE(ILOG,*)'IERR6=',IERR,'  NZ5=',NZ
       C6=DCMPLX(CYR(1),CYI(1))
	    ENDIF
CC
CC   FOR NEGATIVE ORDERS (IU=1) MAKE THE FOLLOWING CHANGES:
CC
        IF(IU.EQ.0) GO TO 10
 	 C1=C1*DCOS(PI*XMU)-C4*DSIN(PI*XMU)
 	 C2=C2*DCOS(PI*XNU)-C5*DSIN(PI*XNU)
       C3=C3*DCOS(PI*XMU)-C6*DSIN(PI*XMU)
 	 C4=C4*DCOS(PI*XMU)+C11*DSIN(PI*XMU)
 	 C5=C5*DCOS(PI*XNU)+C22*DSIN(PI*XNU)
       C6=C6*DCOS(PI*XMU)+C33*DSIN(PI*XMU)
10      CONTINUE
CC
CC   FOR THE LARGE ARGUMENT BESSEL FUNCTIONS IN EQ. (37) OF M AND B 
CC   (2006) THE APPROXIMATIONS GIVEN BY EQ. (43) CAN BE USED. THE 
CC   RATIO J/Y BECOMES COS(Z)/SIN(Z) WHERE Z=B+iA AND CAN BE FURTHER 
CC   SIMPLIFIED (ALGEBRAICALLY), FOR ABSZ>20, TO THE FORM GIVEN BELOW.
CC   (SEE ABRAMOWITZ AND STEGUN 4.3.55 AND 4.3.56)
CC
	 IF(ABSZ.GE.20.D0) THEN
         IF(IU.EQ.1) XMU=-XMU
	   BMU=-XMU*PI/2.D0-PI/4.D0
         C3=CUNITY*DCOS(BMU)-CIMAG*DSIN(BMU)
         C6=CUNITY*DSIN(BMU)+CIMAG*DCOS(BMU)
       ENDIF
CC
CC
CC    NOW COMPUTE EQUATION 42. FIRST CALCULATE ALPHA USING
C      EQUATION 46
	    IF(ABSZ.GE.20.D0.OR.ADM.GE.AXDM) CALPHA=CIMAG
C      OR EQUATIONS 37 OR 45......CONFIRM 45
	    IF(ABSZ.LT.20.D0) CALPHA=-C3/C6
C     CONTINUE...
       CNUM=C1+CALPHA*C4
       CDEN=C2+CALPHA*C5
       CFAC=CIMAG*CDSQRT(ADC*CP/BETA)
	 CRATIO=CNUM/CDEN
	 ABS_NUM=CDABS(CNUM)
	 ABS_DEN=CDABS(CDEN)
 	 IF(ABS_NUM.LT.1.D-40.OR.ABS_DEN.LT.1.D-40) THEN
        CEQ42=CUNITY
       ELSE
	  CEQ42=CFAC*CRATIO
       ENDIF
       CRHS=CEQ42
C
 	 RETURN
       END
CC
CC    FOR NEGATIVE ORDERS,THE APPLY FORMULAE: (CAUTION- THERE ARE RESTRICTIONS)
C
C              J(-FNU,Z) = J(FNU,Z)*COS(PI*FNU) - Y(FNU,Z)*SIN(PI*FNU)
C              Y(-FNU,Z) = Y(FNU,Z)*COS(PI*FNU) + J(FNU,Z)*SIN(PI*FNU)
C     
C     THUS WRITE:
C	IF(IU.EQ.1) THEN
C	 C1=C1*DCOS(PI*XMU)-C4*DSIN(PI*XMU)
C	 C2=C2*DCOS(PI*XNU)-C5*DSIN(PI*XNU)
C      C3=C3*DCOS(PI*XMU)-C6*DSIN(PI*XMU)
C	 C4=C4*DCOS(PI*XMU)+C11*DSIN(PI*XMU)
C	 C5=C5*DCOS(PI*XNU)+C22*DSIN(PI*XNU)
C	 C6=C6*DCOS(PI*XMU)+C33*DSIN(PI*XMU)
C      ENDIF
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE BESKZ1                   *
C      *                                                      *
C      *         VERSION 2.0 CURRENT AS OF 11/10/2009         *
C      *                                                      *
C       ******************************************************
C
C     ORIGINALLY WRITTEN BY JOHN BARKER, BRITISH GEOLOGICAL SURVEY
C     AND REVISED BY ALLEN F. MOENCH TO RETURN UNSCALED VALUES OF K0 AND K1
C
      SUBROUTINE BESKZ1(Z,K0,K1)
      COMPLEX*16 Z,K0,K1
      REAL*8 DBLE,DIMAG,X,Y,RE0,IM0,RE1,IM1
      X=DBLE(Z)
      Y=DIMAG(Z)
      CALL KZEONE(X,Y,RE0,IM0,RE1,IM1)
      RE0=RE0*DEXP(-X)
      RE1=RE1*DEXP(-X)
      IM0=IM0*DEXP(-X)
      IM1=IM1*DEXP(-X)
      K0=DCMPLX(RE0,IM0)
      K1=DCMPLX(RE1,IM1)
      RETURN
      END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE KZEONE                   *
C      *                                                      *
C      *         VERSION 2.0 CURRENT AS OF 11/10/2009         *
C      *                                                      *
C       ******************************************************
C
C     WRITTEN BY JOHN BARKER, BRITISH GEOLOGICAL SURVEY
C
      SUBROUTINE KZEONE(X,Y,RE0,IM0,RE1,IM1)
      DOUBLE PRECISION X,Y,X2,Y2,RE0,IM0,RE1,IM1,
     * R1,R2,T1,T2,P1,P2,RTERM,ITERM,EXSQ(8),TSQ(8)
      DATA TSQ/0.0,3.19303633920635D-1,1.29075862295915,
     * 2.95837445869665,5.40903159724444,8.80407957805676,
     * 1.34685357432515D1,2.02499163658709D1/,
     * EXSQ/0.5641003087264,0.4120286874989,0.1584889157959,
     * 0.3078003387255D-1,0.2778068842913D-2,0.1000044412325D-3,
     * 0.1059115547711D-5,0.1522475804254D-8/
C
      R2=X*X+Y*Y
      IF(X.GT.0.0.OR.R2.NE.0.0) GO TO 10
      WRITE(6,99999)
      RETURN
 10   IF(R2.GE.1.96D2) GO TO 50
      IF(R2.GE.1.849D1) GO TO 30
C
      X2=X/2.0
      Y2=Y/2.0
      P1=X2*X2
      P2=Y2*Y2
      T1=-(DLOG(P1+P2)/2.0+.5772156649015329)
      T2=-DATAN2(Y,X)
      X2=P1-P2
      Y2=X*Y2
      RTERM=1.0
      ITERM=0.0
      RE0=T1
      IM0=T2
      T1=T1+0.5
      RE1=T1
      IM1=T2
      P2=DSQRT(R2)
      L=2.106*P2+4.4
      IF(P2.LT.8.0D-1) L=2.129*P2+4.0
      DO 20 N=1,L
        P1=N
        P2=N*N
        R1=RTERM
        RTERM=(R1*X2-ITERM*Y2)/P2
        ITERM=(R1*Y2+ITERM*X2)/P2
        T1=T1+0.5/P1
        RE0=RE0+T1*RTERM-T2*ITERM
        IM0=IM0+T1*ITERM+T2*RTERM
        P1=P1+1.0
        T1=T1+0.5/P1
        RE1=RE1+(T1*RTERM-T2*ITERM)/P1
        IM1=IM1+(T1*ITERM+T2*RTERM)/P1
 20   CONTINUE
      R1=X/R2-0.5*(X*RE1-Y*IM1)
      R2=-Y/R2-0.5*(X*IM1+Y*RE1)
      P1=DEXP(X)
      RE0=P1*RE0
      IM0=P1*IM0
      RE1=P1*R1
      IM1=P1*R2
      RETURN
C
C
 30   X2=2.0*X
      Y2=2.0*Y
      R1=Y2*Y2
      P1=DSQRT(X2*X2+R1)
      P2=DSQRT(P1+X2)
      T1=EXSQ(1)/(2.0*P1)
      RE0=T1*P2
      IM0=T1/P2
      RE1=0.0
      IM1=0.0
      DO 40 N=2,8
        T2=X2+TSQ(N)
      P1=DSQRT(T2*T2+R1)
      P2=DSQRT(P1+T2)
      T1=EXSQ(N)/P1
      RE0=RE0+T1*P2
      IM0=IM0+T1/P2
      T1=EXSQ(N)*TSQ(N)
      RE1=RE1+T1*P2
      IM1=IM1+T1/P2
 40   CONTINUE
      T2=-Y2*IM0
      RE1=RE1/R2
      R2=Y2*IM1/R2
      RTERM=1.41421356237309*DCOS(Y)
      ITERM=-1.41421356237309*DSIN(Y)
C
      IM0=RE0*ITERM+T2*RTERM
      RE0=RE0*RTERM-T2*ITERM
      T1=RE1*RTERM-R2*ITERM
      T2=RE1*ITERM+R2*RTERM
      RE1=T1*X+T2*Y
      IM1=-T1*Y+T2*X
      RETURN
C
C
 50   RTERM=1.0
      ITERM=0.0
      RE0=1.0
      IM0=0.0
      RE1=1.0
      IM1=0.0
      P1=8.0*R2
      P2=DSQRT(R2)
      L=3.91+8.12D1/P2
      R1=1.0
      R2=1.0
      M=-8
      K=3
      DO 60 N=1,L
        M=M+8
        K=K-M
      R1=DBLE(K-4)*R1
        R2=DBLE(K)*R2
        T1=DBLE(N)*P1
      T2=RTERM
      RTERM=(T2*X+ITERM*Y)/T1
      ITERM=(-T2*Y+ITERM*X)/T1
        RE0=RE0+R1*RTERM
        IM0=IM0+R1*ITERM
        RE1=RE1+R2*RTERM
        IM1=IM1+R2*ITERM
 60   CONTINUE
      T1=DSQRT(P2+X)
      T2=-Y/T1
      P1=8.86226925452758D-1/P2
C
      RTERM=P1*DCOS(Y)
      ITERM=-P1*DSIN(Y)
      R1=RE0*RTERM-IM0*ITERM
      R2=RE0*ITERM+IM0*RTERM
      RE0=T1*R1-T2*R2
      IM0=T1*R2+T2*R1
      R1=RE1*RTERM-IM1*ITERM
      R2=RE1*ITERM+IM1*RTERM
      RE1=T1*R1-T2*R2
      IM1=T1*R2+T2*R1
      RETURN
99999 FORMAT(' ARGUMENT OF THE BESSEL FUNCTIONS IS ZERO',
     * /,' OR LIES IN LEFT HALF COMPLEX PLANE')
      END
C
C
C       ******************************************************
C      *                                                      *
C      *                  SUBROUTINE LAPADC                   *
C      *                                                      *
C      *         VERSION 2.0 CURRENT AS OF 11/10/2009         *
C      *                                                      *
C       ******************************************************
C
C
      SUBROUTINE LAPADC(T,GAMMA,BIGT,N,F,FUN,METH,INIT,D,WORK)
C
C-----------------------------------------------------------------------
C
C     AUTHOR
C
C         J.H. KNIGHT
C         DIVISION OF ENVIRONMENTAL MECHANICS
C         CSIRO, CANBERRA, ACT 2601, AUSTRALIA
C         AUGUST, 1986
C
C     REFERENCES
C
C         DE HOOG, F.R., KNIGHT, J.H. AND STOKES, A.N.
C         AN IMPROVED METHOD FOR NUMERICAL INVERSION OF LAPLACE
C         TRANSFORMS. SIAM J. SCI. STAT. COMPUT., 3, PP. 357-366, 1982.
C
C         DE HOOG, F.R., KNIGHT, J.H. AND STOKES, A.N.
C         SUBROUTINE LAPACC FOR LAPLACE TRANSFORM INVERSION BY
C         ACCELERATION OF CONVERGENCE OF COMPLEX SUM.
C         ACM TRANS. MATH. SOFTWARE, (MANUSCRIPT IN PREPARATION).
C
C     ABSTRACT
C
C         LAPADC COMPUTES AN APPROXIMATION TO THE INVERSE LAPLACE
C         TRANSFORM F(T) CORRESPONDING TO THE LAPLACE TRANSFORM
C         FBAR(P), USING A RATIONAL APPROXIMATION TO THE FOURIER
C         SERIES RESULTING WHEN THE INVERSION INTEGRAL IS
C         DISCRETIZED USING THE TRAPEZOIDAL RULE.
C         THE RATIONAL APPROXIMATION IS A DIAGONAL PADE APPROXIMATION
C         COMPUTED USING EITHER THE EPSILON ALGORITHM, IN WHICH THE
C         EPSILON TABLE MUST BE RECOMPUTED AT EACH VALUE OF T, OR
C         USING THE QUOTIENT-DIFFERENCE ALGORITHM, IN WHICH THE
C         QUOTIENT-DIFFERENCE ALGORITHM IS USED TO COMPUTE THE
C         COEFFICIENTS OF THE ASSOCIATED CONTINUED FRACTION, AND THE
C         CONTINUED FRACTION IS EVALUATED AT EACH VALUE OF T.
C         IN ADDITION, THE EVALUATION OF THE CONTINUED FRACTION IS
C         MADE MORE ACCURATE WITH AN IMPROVED ESTIMATE OF THE
C         REMAINDER.
C
C     METHOD
C
C         A REAL VALUED INVERSE FUNCTION F(T) IS THE REAL PART OF THE
C         INTEGRAL
C                                INFINITY
C
C         (ONE/PI)*EXP(GAMMA*T)* INTEGRAL FBAR(GAMMA+I*W)*EXP(I*W*T)*DW
C
C                                W=ZERO
C
C         WITH FBAR THE LAPLACE TRANSFORM, GAMMA A REAL PARAMETER, AND
C         I**2=-1.  W IS THE VARIABLE OF INTEGRATION.
C         IF THIS IS DISCRETIZED USING THE TRAPEZOIDAL RULE WITH STEP
C         SIZE PI/BIGT, A FOURIER SERIES RESULTS, AND F(T) IS THE SUM
C         OF THE DISCRETIZATION ERROR
C
C               INFINITY
C
C               SUM     EXP(-TWO*GAMMA*K*BIGT)*F(T+TWO*K*BIGT)
C
C               K=1
C
C         AND THE REAL PART OF THE FOURIER SERIES
C
C                                    INFINITY
C
C           (ONE/BIGT)*EXP(GAMMA*T)* SUM  A(K)*EXP(I*K*PI*T/BIGT)
C
C                                    K=0
C
C         WITH THE COEFFICIENTS (A(K),K=0,INFINITY) GIVEN BY
C
C              A(0) = HALF*FBAR(GAMMA),
C
C              A(K) = FBAR(GAMMA+I*K*PI/BIGT),   K=1,INFINITY.
C
C         THIS EXPRESSION CAN ALSO BE CONSIDERED AS A POWER SERIES IN
C         THE VARIABLE Z=EXP(I*PI*T/BIGT), AND WE INTRODUCE A
C         TRUNCATION ERROR WHEN WE USE A FINITE POWER SERIES
C
C                        M2
C
C              V(Z,M2) = SUM  A(K)*Z**K,       M2 = M*2.
C
C                        K=0
C
C         THE EPSILON ALGORITHM CALCULATES THE DIAGONAL PADE
C         APPROXIMATION TO THIS FINITE POWER SERIES
C
C                        M                M
C
C              V(Z,M2) = SUM  B(K)*Z**K / SUM  C(K)*Z**K
C
C                        K=0              K=0
C
C                                    + HIGHER ORDER TERMS,   C(0)=ONE.
C
C         THE CALCULATION MUST BE DONE FOR EACH NEW VALUE OF T.
C         THIS METHOD IS USED FOR METH=1.
C
C         THE QUOTIENT-DIFFERENCE (QD) ALGORITHM IS ANOTHER WAY
C         OF CALCULATING THIS DIAGONAL PADE APPROXIMATION, IN THE FORM
C         OF A CONTINUED FRACTION
C
C         V(Z,M2) = D(0)/(ONE+D(1)*Z/(ONE+...+D(M2-1)*Z/(ONE+R2M(Z)))),
C
C         WHERE R2M(Z) IS THE UNKNOWN REMAINDER.
C         THE QD ALGORITHM GIVES THE COEFFICIENTS (D(K),K=0,M2), AND
C         THE CONTINUED FRACTION IS THEN EVALUATED BY FORWARD RECURRENCE,
C         FOR EACH NEW VALUE OF T, WITH R2M(Z) TAKEN AS D(M2)*Z.
C         THE ANSWERS SHOULD BE IDENTICAL TO THOSE GIVEN BY THE
C         EPSILON ALGORITHM, APART FROM ROUNDOFF ERROR.
C         THIS METHOD IS USED FOR METH=2.
C
C         IN ADDITION, THE CONVERGENCE OF THE CONTINUED FRACTION CAN
C         BE ACCELERATED WITH AN IMPROVED ESTIMATE OF THE REMAINDER,
C         SATISFYING
C
C               R2M + ONE + (D(M2-1)-D(M2))*Z = D(M2)*Z/R2M
C
C         THIS METHOD IS USED FOR METH=3.
C
C     USAGE
C
C         THE USER MUST CHOOSE THE PARAMETERS BIGT AND GAMMA WHICH
C         DETERMINE THE DISCRETIZATION ERROR, AND N(=M2) WHICH
C         DETERMINES THE TRUNCATION ERROR AND ROUNDOFF ERROR, GIVEN
C         THE CHOICE OF BIGT AND GAMMA.  AS N IS INCREASED, THE
C         TRUNCATION ERROR DECREASES BUT THE ROUNDOFF ERROR INCREASES,
C         SINCE THE EPSILON AND QD ALGORITHMS ARE NUMERICALLY UNSTABLE.
C         IT IS DESIRABLE TO USE DOUBLE PRECISION FOR THE CALCULATIONS,
C         UNLESS THE COMPUTER KEEPS AT LEAST 12 SIGNIFICANT DECIMAL
C         DIGITS IN SINGLE PRECISION.
C         METH=1 USES THE EPSILON ALGORITHM, METH=2 USES THE QUOTIENT-
C         DIFFERENCE ALGORITHM, AND METH=3 USES THE QD ALGORITHM WITH
C         IMPROVED ESTIMATE OF THE REMAINDER ON EVALUATION.
C
C         AN ESTIMATE OF THE OPTIMAL VALUE OF N FOR GIVEN BIGT AND
C         GAMMA CAN BE GOT BY VARYING N AND CHOOSING A VALUE SLIGHTLY
C         LESS THAN THAT WHICH CAUSES THE RESULTS FOR METH=1 TO VARY
C         SIGNIFICANTLY FROM THOSE FOR METH=2.
C
C         CALCULATIONS USING THE QD ALGORITHM ARE MOST EFFICIENT
C         AND MOST ACCURATE IF LAPADC IS CALLED WITH METH=3, AND WITH
C         INIT=1 ON THE FIRST CALL WITH NEW VALUES OF BIGT, GAMMA, N
C         OR FUN, AND WITH INIT=2 ON SUBSEQUENT CALLS WITH NEW VALUES
C         OF T AND THE OTHER PARAMETERS UNCHANGED.
C
C
C     DESCRIPTION OF ARGUMENTS
C
C         T      -  REAL (DOUBLE PRECISION) VARIABLE.  ON INPUT, VALUE
C                   OF INDEPENDENT VARIABLE AT WHICH INVERSE FUNCTION
C                   F(T) IS TO BE APPROXIMATED.  UNCHANGED ON OUTPUT.
C         GAMMA  -  REAL (DOUBLE PRECISION) VARIABLE.  ON INPUT,
C                   CONTAINS PARAMETER OF INVERSION INTEGRAL, GOVERNING
C                   ACCURACY OF INVERSION.  UNCHANGED ON OUTPUT.
C         BIGT   -  REAL (DOUBLE PRECISION) VARIABLE.  ON INPUT,
C                   CONTAINS PARAMETER USED TO DISCRETIZE INVERSION
C                   INTEGRAL. GOVERNS DISCRETIZATION ERROR.
C                   UNCHANGED ON OUTPUT.
C         N      -  INTEGER VARIABLE, SHOULD BE EVEN .GE. 2.  ON INPUT,
C                   CONTAINS NUMBER OF TERMS TO BE USED IN SUM FOR
C                   APPROXIMATION.  DETERMINES TRUNCATION ERROR.
C                   UNCHANGED ON OUTPUT.
C         F      -  REAL (DOUBLE PRECISION) VARIABLE.  ON OUTPUT,
C                   CONTAINS VALUE OF INVERSE FUNCTION F(T), EVALUATED
C                   AT T.
C         FUN    -  NAME OF USER-SUPPLIED SUBROUTINE WHICH EVALUATES
C                   (DOUBLE) COMPLEX VALUES OF TRANSFORM FBAR(P)
C                   FOR (DOUBLE) COMPLEX VALUES OF LAPLACE
C                   TRANSFORM VARIABLE P.  CALLED IF METH .EQ. 1 .OR.
C                   INIT .EQ. 1.  MUST BE DECLARED EXTERNAL IN
C                   (SUB)PROGRAM CALLING LAPADC.
C         METH   -  INTEGER VARIABLE, DETERMINING METHOD TO BE USED TO
C                   ACCELERATE CONVERGENCE OF SUM.
C                   ON INPUT, METH .EQ. 1 INDICATES THAT THE EPSILON
C                   ALGORITHM IS TO BE USED.
C                   METH .EQ. 2 INDICATES THAT THE ORDINARY QUOTIENT
C                   DIFFERENCE ALGORITHM IS TO BE USED.
C                   METH .EQ. 3 INDICATES THAT THE QUOTIENT DIFFERENCE
C                   ALGORITHM IS TO BE USED, WITH FURTHER ACCELERATION
C                   OF THE CONVERGENCE OF THE CONTINUED FRACTION.
C                   UNCHANGED ON OUTPUT.
C         INIT   -  INTEGER VARIABLE. ON INPUT, USED ONLY IF Q-D
C                   ALGORITHM IS SPECIFIED (METH .EQ. 2 OR 3).
C                   INIT .EQ. 1 INDICATES THAT THIS IS THE FIRST CALL
C                   OF THE ALGORITHM WITH THESE VALUES OF GAMMA, BIGT,
C                   N, FUN.  IN THIS CASE, THE VALUES OF THE CONTINUED
C                   FRACTION COEFFICIENTS D(0:N) ARE RECALCULATED, AND
C                   THE CONTINUED FRACTION IS EVALUATED.
C                   INIT .NE. 1 INDICATES THAT THE VALUES OF GAMMA,
C                   BIGT, N, AND FUN ARE UNCHANGED SINCE THE LAST CALL.
C                   IN THIS CASE, THE INPUT VALUES OF THE CONTINUED
C                   FRACTION COEFFICIENTS D(0:N) ARE USED, AND THE
C                   CONTINUED FRACTION IS EVALUATED.
C                   UNCHANGED ON OUTPUT.
C         D      -  (DOUBLE) COMPLEX ARRAY, OF VARIABLE DIMENSION (0:N).
C                   MUST BE DIMENSIONED IN CALLING PROGRAM.
C                   ON INPUT, IF METH .EQ. 2 OR 3 .AND. INIT .NE. 1,
C                   MUST CONTAIN CONTINUED FRACTION COEFFICIENTS
C                   OUTPUT ON A PREVIOUS CALL.
C                   ON OUTPUT, IF METH .EQ. 2 OR 3, CONTAINS CONTINUED
C                   FRACTION COEFFICIENTS CALCULATED ON THIS CALL
C                   (INIT .EQ. 1) OR AS SUPPLIED ON INPUT (INIT .NE. 1).
C         WORK   -  (DOUBLE) COMPLEX ARRAY OF VARIABLE DIMENSION (0:N).
C                   MUST BE DIMENSIONED IN CALLING PROGRAM.
C                   WORK SPACE USED FOR CALCULATING SUCCESSIVE DIAGONALS
C                   OF THE TABLE IF METH .EQ. 1 .OR. INIT .EQ. 1.
C                   ON OUTPUT, CONTAINS LAST CALCULATED DIAGONAL OF
C                   TABLE.
C
C     SUBROUTINE CALLED
C
C           FUN(P,FBAR) - USER-SUPPLIED SUBROUTINE WHICH EVALUATES
C                   (DOUBLE) COMPLEX VALUES OF TRANSFORM FBAR(P)
C                   FOR (DOUBLE) COMPLEX VALUES OF LAPLACE
C                   TRANSFORM VARIABLE P.  CALLED IF METH .EQ. 1 .OR.
C                   INIT .EQ. 1.  MUST BE DECLARED EXTERNAL IN
C                   (SUB)PROGRAM WHICH CALLS SUBROUTINE LAPADC.
C
C-----------------------------------------------------------------------
C
C     THIS IS THE DOUBLE COMPLEX (NON ANSI STANDARD) VERSION.
C
C-----------------------------------------------------------------------
C
      IMPLICIT COMPLEX*16 (A-H,O-Z)
      DOUBLE PRECISION T,GAMMA,BIGT,F,PI,FACTOR,ZEROR,ARGR,ARGI,RESULT
     *                ,ZR,ZI
C     IMPLICIT COMPLEX (A-H,O-Z)
C     REAL T,GAMMA,BIGT,F,PI,FACTOR,ZEROR,ARGR,ARGI,RESULT,ZR,ZI
      DIMENSION D(0:N),WORK(0:N)
      PARAMETER (ZERO=(0.D+00,0.D+00), HALF=(0.5D+00,0.0D+00)
     *          ,ONE=(1.0D+00,0.0D+00), ZEROR=0.0D+00
     *          ,PI=3.14159265358979323846264338327950D+00)
C     PARAMETER (ZERO=(0.E+00,0.E+00), HALF=(0.5E+00,0.0E+00)
C    *          ,ONE=(1.0E+00,0.0E+00), ZEROR=0.0E+00
C    *          ,PI=3.1415926535897932385E+00)
      IF ( N .LT. 2 ) RETURN
C
C     MAKE ORDER A MULTIPLE OF 2
C
      M2 = (N/2)*2
      FACTOR = PI/BIGT
      ARGR = T*FACTOR
      ZR = DCOS(ARGR)
      ZI = DSIN(ARGR)
      Z = DCMPLX(ZR,ZI)
C     Z = CMPLX(ZR,ZI)
      IF ( METH .EQ. 1 .OR. INIT .EQ. 1 ) THEN
C
C       CALCULATE TABLE IF EPSILON ALGORITHM IS TO BE USED, OR IF THIS
C       IS THE FIRST CALL TO QUOTIENT-DIFFERENCE ALGORITHM WITH THESE
C       PARAMETERS AND INVERSE TRANSFORM.
C
        ARG = DCMPLX(GAMMA,ZEROR)
C       ARG = CMPLX(GAMMA,ZEROR)
        CALL FUN(ARG,A)
        AOLD = HALF*A
        ARGI = FACTOR
        ARG = DCMPLX(GAMMA,ARGI)
C       ARG = CMPLX(GAMMA,ARGI)
        CALL FUN(ARG,A)
C
C       INITIALIZE TABLE ENTRIES
C
        IF ( METH .EQ. 1 ) THEN
          ZTOJ = Z
          TERM = A*ZTOJ
          WORK(0) = AOLD + TERM
          WORK(1) = ONE/TERM
        ELSE
          D(0) = AOLD
          WORK(0) = ZERO
          WORK(1) = A/AOLD
          D(1) = -WORK(1)
          AOLD = A
        ENDIF
C
C       CALCULATE SUCCESSIVE DIAGONALS OF TABLE
C
        DO 20 J = 2,M2
C
C         INITIALIZE CALCULATION OF DIAGONAL
C
          OLD2 = WORK(0)
          OLD1 = WORK(1)
          ARGI = ARGI + FACTOR
          ARG = DCMPLX(GAMMA,ARGI)
C         ARG = CMPLX(GAMMA,ARGI)
          CALL FUN(ARG,A)
          IF ( METH .EQ. 1 ) THEN
C
C           CALCULATE NEXT TERM AND SUM OF POWER SERIES
C
            ZTOJ = Z*ZTOJ
            TERM = A*ZTOJ
            WORK(0) = WORK(0) + TERM
            WORK(1) = ONE/TERM
          ELSE
            WORK(0) = ZERO
            WORK(1) = A/AOLD
            AOLD = A
          ENDIF
C
C         CALCULATE DIAGONAL USING RHOMBUS RULES
C
          DO 10 I = 2,J
            OLD3 = OLD2
            OLD2 = OLD1
            OLD1 = WORK(I)
            IF( METH .EQ. 1 ) THEN
C
C             EPSILON ALGORITHM RULE
C
              WORK(I) = OLD3 + ONE/(WORK(I-1) - OLD2)
            ELSE
C
C             QUOTIENT DIFFERENCE ALGORITHM RULES
C
              IF ( (I/2)*2 .EQ. I ) THEN
C
C               DIFFERENCE FORM
C
                WORK(I) = OLD3  +  (WORK(I-1) - OLD2)
              ELSE
C
C               QUOTIENT FORM
C
                WORK(I) = OLD3  *  (WORK(I-1) / OLD2)
              ENDIF
            ENDIF
 10       CONTINUE
C
C         SAVE CONTINUED FRACTION COEFFICIENTS
C
          IF ( METH .NE. 1 ) D(J) = -WORK(J)
 20     CONTINUE
      ENDIF
      IF ( METH .EQ. 1 ) THEN
C
C       RESULT OF EPSILON ALGORITHM COMPUTATION
C
        RESULT = DBLE(WORK(M2))
C       RESULT = REAL(WORK(M2))
      ELSE
C
C       EVALUATE CONTINUED FRACTION
C
C       INITIALIZE RECURRENCE RELATIONS
C
        AOLD2 = D(0)
        AOLD1 = D(0)
        BOLD2 = ONE
        BOLD1 = ONE + D(1)*Z
C
C       USE RECURRENCE RELATIONS
C
        DO 30 J = 2,M2
          IF ( METH .EQ. 3 .AND. J .EQ. M2 ) THEN
C
C           FURTHER ACCELERATION ON LAST ITERATION IF REQUIRED
C
            H2M = HALF*(ONE + (D(M2-1) - D(M2))*Z)
            R2M = -H2M*(ONE - CDSQRT((ONE + D(M2)*Z/(H2M*H2M))))
            A = AOLD1 + R2M*AOLD2
            B = BOLD1 + R2M*BOLD2
          ELSE
            A = AOLD1 + D(J)*Z*AOLD2
            AOLD2 = AOLD1
            AOLD1 = A
            B = BOLD1 + D(J)*Z*BOLD2
            BOLD2 = BOLD1
            BOLD1 = B
          ENDIF
 30     CONTINUE
C
C       RESULT OF QUOTIENT DIFFERENCE ALGORITHM EVALUATION
C
        RESULT = DBLE(A/B)
C       RESULT = REAL(A/B)
      ENDIF
C
C     CALCULATE REQUIRED APPROXIMATE INVERSE
C
      F = DEXP(GAMMA*T)*RESULT/BIGT
      RETURN
      END
C
C