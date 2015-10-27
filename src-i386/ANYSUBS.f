      SUBROUTINE GETDEN(REF,DI,DENRAT,ECHO,CHANGE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL ECHO,CHANGE
      DENRAT = 0.0D0
      IDENRAT = 1
      DATA HALF/0.5D0/,P01/0.01D0/,ONE/1.D0/
      ERRMAX = ONE
      CHANGE = .FALSE.
      DO 30 IDENRAT = 1, 64
      DENRAT = DENRAT + 1.0D0
      REFDEN = REF * DENRAT
      DIDEN  = DI  * DENRAT
      IF (DIDEN .GT. 128) GO TO 30
      ERROR = ABS(MOD(REFDEN+P01,ONE) - P01) +
     1    ABS(MOD(DIDEN+P01,ONE) - P01)
      IF (ERROR .LT. P01) GO TO 40
      IF (ERROR .LT. ERRMAX) THEN
        ERRMAX = ERROR
        USEDEN = DENRAT
        USEREF = INT(REFDEN + HALF) / DENRAT
        USEDI  = INT(DIDEN  + HALF) / DENRAT
        ENDIF
 30   CONTINUE
c      IF (ECHO) WRITE(*,101) INT(USEDEN)
 101  FORMAT(' CHOICES NOT RATIONAL WITH SMALL ENOUGH DENOMINATOR'/
     1 ' THEY WILL BE EDITED TO THE CLOSEST MANAGEABLE DENOMINATOR',I4)
      REF = USEREF
      DI  = USEDI
      DENRAT = USEDEN
 40   CONTINUE
      RETURN
      END
C
      SUBROUTINE CALCUS(DI,REF,NDIS,DENRAT,ARG2,WL,WU,
     1 REGARL,FIRARL,SSARL,EPS,ESTERR,IFAULT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL NORMAL,VARUP,VARDN,POIUP,POIDN,BINUP,BINDN,
     1 NBINUP,NBINDN,GAUIUP,GAUIDN
      GO TO (310,320,330,340,350,360,370,380,390,400,410), NDIS
 310  CALL CUSRLG(DI, REF,NORMAL,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 320  CALL CUSRLG(DI, REF,VARUP ,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 330  CALL CUSRLG(DI, REF,VARDN ,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 340  CALL CUSRLG(DI, REF,POIUP ,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 350  CALL CUSRLG(DI, REF,POIDN ,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 360  CALL CUSRLG(DI, REF,BINUP ,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 370  CALL CUSRLG(DI, REF,BINDN ,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 380  CALL CUSRLG(DI, REF,NBINUP,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 390  CALL CUSRLG(DI, REF,NBINDN,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 400  CALL CUSRLG(DI, REF,GAUIUP,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 410  CALL CUSRLG(DI, REF,GAUIDN,DENRAT,ARG2,WL,WU,REGARL,
     1 FIRARL,SSARL,EPS,ESTERR,IFAULT)
      GO TO 500
 500  CONTINUE
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION NORMAL(ARG,ARG2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CUMDIS = PHI(ARG)
      NORMAL = CUMDIS
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION VARUP(ARG,ARG2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PARMS/ REALNO, NDF
      DF = NDF
      CUMDIS = 0
      IF (ARG .LT. 0) GO TO 10
      ARG3 = ARG * DF
      CUMDIS = 1 - CHISQ(ARG3,NDF)
 10   CONTINUE
      VARUP = CUMDIS
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION VARDN(ARG,ARG2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PARMS/ REALNO, NDF
      DF = NDF
      CUMDIS = 1
      IF (ARG .GT. 0) GO TO 10
      ARG3 = -ARG * DF
      CUMDIS = CHISQ(ARG3,NDF)
 10   CONTINUE
      VARDN = CUMDIS
      RETURN
      END
C     
      DOUBLE PRECISION FUNCTION POIUP(ARG,ARG2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CUMDIS = 0
      IF (ARG .LT. 0) GO TO 10
      INX = ARG + .5
      T0 = EXP(-ARG2)
      DO 1 I = 0, INX
      CUMDIS = CUMDIS + T0
      T0 = T0 * ARG2 / (I+1)
 1    CONTINUE
 10   CONTINUE
      POIUP = CUMDIS
      RETURN
      END
C     
      DOUBLE PRECISION FUNCTION POIDN(ARG,ARG2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CUMDIS = 1
      IF (ARG .GE. 0) GO TO 10
      AARG = ABS(ARG)
      INX = AARG - .5
      T0 = EXP(-ARG2)
      DO 1 I = 0, INX
      CUMDIS = CUMDIS - T0
      T0 = T0 * ARG2 / (I+1)
 1    CONTINUE
 10   CONTINUE
      POIDN = CUMDIS
      RETURN
      END
C     
      DOUBLE PRECISION FUNCTION BINUP(ARG,ARG2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PARMS/ REALNO, NBIG
      DATA ONE/1.D0/
      BIGN = NBIG
      CUMDIS = 0
      IF (ARG .LT. 0) GO TO 10
      INX = ARG + .5
      T0 = (ONE - ARG2) ** NBIG
      DO 1 I = 0, INX
      FI = I
      CUMDIS = CUMDIS + T0
      T0 = T0 * (BIGN - FI) * ARG2 / ((FI + ONE ) * (ONE - ARG2))
 1    CONTINUE
 10   CONTINUE
      BINUP = CUMDIS
      RETURN
      END
C     
      DOUBLE PRECISION FUNCTION BINDN(ARG,ARG2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PARMS/ REALNO, NBIG
      DATA ONE /1.D0/
      BIGN = NBIG
      CUMDIS = 1
      IF (ARG .GE. 0) GO TO 10
      AARG = ABS(ARG)
      INX = AARG - .5
      T0 = (ONE - ARG2) ** NBIG
      DO 1 I = 0, INX
      FI = I
      CUMDIS = CUMDIS - T0
      T0 = T0 * (BIGN - FI) * ARG2 / ((FI + ONE) * (ONE - ARG2))
 1    CONTINUE
 10   CONTINUE
      BINDN = CUMDIS
      RETURN
      END
C     
      DOUBLE PRECISION FUNCTION NBINUP(ARG,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PARMS/ R, INTEG
      DATA ZERO/0.D0/, ONE/1.D0/
      CUMDIS = 0
      IF (ARG .LT. 0) GO TO 10
      INX = ARG + .5
      T0 = (C / (ONE + C)) ** R
      CUMDIS = ZERO
      DO 1 I = 0, INX
      FI = I
      CUMDIS = CUMDIS + T0
      T0 = T0 * (R + FI) / ((FI + ONE) * (ONE + C))
 1    CONTINUE
 10   CONTINUE
      NBINUP = CUMDIS
      RETURN
      END
C     
      DOUBLE PRECISION FUNCTION NBINDN(ARG,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PARMS/ R, INTEG
      DATA ONE /1.D0/
      CUMDIS = 1
      IF (ARG .GE. 0) GO TO 10
      AARG = ABS(ARG)
      INX = AARG - .5
      T0 = (C / (ONE + C)) ** R
      DO 1 I = 0, INX
      FI = I
      CUMDIS = CUMDIS - T0
      T0 = T0 * (R + FI) / ((FI + ONE) * (ONE + C))
 1    CONTINUE
 10   CONTINUE
      NBINDN = CUMDIS
      RETURN
      END
C     
      DOUBLE PRECISION FUNCTION GAUIUP(ARG, AMU)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PARMS/ ALAM, INTEG
      DATA ZERO/0.D0/, ONE/1.D0/, TWO/2.D0/
      GAUIUP = ZERO
      IF (ARG .LE. ZERO) RETURN
      RUTER = SQRT(ALAM / ARG)
      TBYMU = ARG / AMU
      GAUIUP = ONE - PHI(RUTER * (ONE - TBYMU)) + EXP(TWO * ALAM / AMU)
     1 * PHI(-RUTER * (ONE + TBYMU))
      RETURN
      END
C     
      DOUBLE PRECISION FUNCTION GAUIDN(ARG,AMU)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /PARMS/ ALAM, INTEG
      DATA ZERO/0.D0/, ONE/1.D0/, TWO/2.D0/
      GAUIDN = ONE
      IF (ARG .GE. ZERO) RETURN
      RUTER = SQRT(-ALAM / ARG)
      TBYMU = -ARG / AMU
      GAUIDN =  PHI(RUTER * (ONE - TBYMU)) - EXP(TWO * ALAM / AMU)
     1 * PHI(-RUTER * (ONE + TBYMU))
      RETURN
      END
      FUNCTION PHI(X)
C
C     ALGORITHM FOR THE NORMAL INTEGRAL
C     ADAPTED FROM APPLIED STATISTICS ALGORITHM 66
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DOUBLE PRECISION LTONE,UTZERO,ZERO,HALF,ONE,CON,Z,Y,X
      LOGICAL UP
      DATA LTONE/8.5D0/, UTZERO/13.D0/
      DATA ZERO,HALF,ONE,CON/0.D0,0.5D0,1.D0,1.28D0/
      UP = .FALSE.
      Z = X
      IF (Z .GE. ZERO) GO TO 10
      UP = .TRUE.
      Z = -Z
 10   IF (Z .LE. LTONE .OR. UP .AND. Z .LE. UTZERO) GO TO 20
      PHI = ZERO
      GO TO 40
 20   Y = HALF * Z * Z
      IF (Z .GT. CON) GO TO 30
      PHI = HALF - Z * (0.398942280444D0 - 0.399903438504D0 * Y /
     1 (Y + 5.75885480458D0 - 29.8213557808D0 /
     2 (Y + 2.62433121679D0 + 48.6959930692D0 /
     3 (Y + 5.92885724438D0))))
      GO TO 40
 30   PHI = 0.398942280385D0 * EXP(-Y) /
     1 (Z - 3.8052D-8 + 1.00000615302D0 /
     2 (Z + 3.98064794D-4 + 1.98615381364D0 /
     3 (Z - 0.151679116635D0 + 5.29330324926D0 /
     4 (Z + 4.8385912808D0 - 15.1508972451D0 /
     5 (Z + 0.742380924027D0 + 30.789933034D0 /
     6 (Z + 3.99019417011D0))))))
 40   IF (.NOT. UP) PHI = ONE - PHI
      RETURN
      END
      FUNCTION CHISQ(X,IDF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     EVALUATES THE RIGHT TAIL AREA OF THE CHI-SQUARED DISTRIBUTION
C     WITH IDF DEGREES OF FREEDOM, EVALUATED AT X.  THE METHOD IS
C     RECURSIVE.
      DATA ONE/1.D0/, TINY/1.D-12/, TWO/2.D0/, HALF/0.5D0/,
     1 TERDIV/1.2533141373155D0/
      IF (X .GT. TINY) GO TO 10
      CHISQ = ONE
      RETURN
 10   CONTINUE
      LOW = MOD(IDF+1,2) + 1
      IF (LOW .EQ. 2) GO TO 1
      RUTX = DSQRT (X)
      CHISQ = TWO * PHI (-RUTX)
      IF (IDF .EQ. 1) RETURN
      TERM = RUTX * EXP(-HALF * X) / TERDIV
      FI = ONE
      GO TO 3
 1    CONTINUE
      CHISQ = EXP ( -HALF * X)
      IF (IDF .EQ. 2) RETURN
      FI = TWO
      TERM = CHISQ * X * HALF
 3    CONTINUE
      LOW = LOW + 2
      DO 4 I = LOW, IDF, 2
      CHISQ = CHISQ + TERM
      FI = FI + 2
      TERM = TERM * X / FI
 4    CONTINUE
      RETURN
      END
C      
      SUBROUTINE CUSRLG (DI,REF,CUMDIS,DENRAT,ARG2,WINSRL,WINSRU,
     1 REGARL,FIRARL,SSARL,EPS,ESTERR,IFAULT)
C
C     SUBROUTINE FOR THE CALCULATION OF AVERAGE RUN LENGTHS OF CUSUMS
C
C     DOUGLAS M HAWKINS
C     DEPARTMENT OF APPLIED STATISTICS
C     UNIVERSITY OF MINNESOTA
C
C     CODE AS OF DECEMBER 1993,  INCORPORATES CORRECTIONS TO THE CODE
C     HAWKINS (1992) COMMUNICATIONS IN STATISTICS SIMULATION AND
C     COMPUTATION, PAGES 1001-1020
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXPOW=512, MAXHLF=9, TOLER=1.0D-12)
      DIMENSION TRAN(0:MAXPOW+1,0:MAXPOW+1), ANSWER(0:MAXPOW+1),
     1COPY(0:MAXPOW+1 ,0:MAXPOW+1), PHITAB(-2*MAXPOW-2:2*MAXPOW+2),
     2RICHAR(MAXHLF,MAXHLF,2), STEADY(0:MAXPOW+1), WORK(0:MAXPOW+1)
      LOGICAL ISREAL
      EXTERNAL CUMDIS
      DATA ZERO/0.D0/,HALF/0.5D0/,ONE/1.D0/,TWO/2.D0/,FOUR/4.D0/,FIVE/5.
     1D0/,SIX/6.D0/,P01/0.01D0/
C     WRITE(*,*) 'DI,REF,WINSRL,WINSRU',DI,REF,WINSRL,WINSRU
      IFAULT=0
      REGARL=0
      FIRARL=0
      SSARL =0
      ESTERR = ONE
      DO 5 I = -2*MAXPOW-2, 2*MAXPOW+2
 5    PHITAB(I) = ZERO
C
C     CHECK PARAMETERS FOR FEASIBILITY
C
      IF (DI.LT.0) IFAULT=1
C     IF (WINSRU.LE.REF) IFAULT=IFAULT+2
      IF (DENRAT.LT.ZERO) IFAULT=IFAULT+8
      ISREAL = DENRAT .EQ. ZERO
      MBIG=1
      IDENRN = 0
      IF (.NOT. ISREAL) THEN
         KNUMER = ABS(REF) * DENRAT + HALF
         IF (REF .LE. ZERO) KNUMER = -KNUMER
         IDENRN = DENRAT
         MBIG = DI*DENRAT-HALF
         IWINTG = ABS(WINSRU) * DENRAT + HALF
         IWINTL = ABS(WINSRL) * DENRAT + HALF
         IF (WINSRU .LT. ZERO) IWINTG = -IWINTG
         IF (WINSRL .LT. ZERO) IWINTL = -IWINTL
         MB1 = MBIG + 1
         IF (MBIG.GE.MAXPOW) IFAULT=IFAULT+16
         DEVIAT = ABS(FLOAT(KNUMER)-REF*DENRAT)+ABS(FLOAT(MBIG)-
     1     DI*DENRAT+ONE)+ABS(FLOAT(IWINTG)-WINSRU*DENRAT)+
     2     ABS(FLOAT(IWINTL)-WINSRL*DENRAT)
         IF (DEVIAT .GT. P01) IFAULT = IFAULT + 32
         ENDIF
      IF (IFAULT.NE.0) GO TO 130
      ANSWER(0)=0
C
C     SET UP COEFFICIENT MATRIX OF LINEAR EQUATIONS
C
      DO 110 MLOOP=1,MAXHLF
        IF (ISREAL) THEN
          MBIG=2*MBIG
          MB1=MBIG+1
          DELTA=DI/MBIG
          MTWO=2*MBIG
          LASTNX = MTWO + 2
          FACT=FOUR
          ELSE
          LASTNX = MBIG
          ENDIF
        DO 10 I=-LASTNX-IDENRN,LASTNX
          IF (ISREAL) THEN
            FACT=FIVE-FACT
            ARG=HALF*DELTA*I+REF
            IF (ARG.GT.WINSRU) THEN
              PHITAB(I)=FACT/SIX
            ELSE
              PHITAB(I) = ZERO
              IF (ARG .GE. WINSRL) PHITAB(I)=CUMDIS(ARG,ARG2)*FACT/SIX
            ENDIF
          ELSE
          IARG = I+KNUMER
          IF (MOD(IARG,IDENRN) .EQ. 0) THEN
             ARG = IARG/IDENRN
             PHITAB(I) = ONE
             IF (ARG .LT. WINSRU) THEN
               PHITAB(I) = 0
               IF (ARG .GE. WINSRL) PHITAB(I) = CUMDIS(ARG,ARG2)
               ENDIF
             ELSE
             PHITAB(I) = PHITAB(I-1)
             IF(IARG .GE. IWINTG) PHITAB(I) = ONE
             IF(IARG .LT. IWINTL) PHITAB(I) = ZERO
             ENDIF
          ENDIF
C       WRITE(*,'(I5,2F10.5)') I,ARG,PHITAB(I)
 10     CONTINUE

        DO 30 J=1,MBIG
          IF (ISREAL) THEN
            INX=2*J
            TRAN(J,0)=PHITAB(-INX)+PHITAB(-INX+1)+PHITAB(-INX+2)
            TRAN(0,J)=SIX*(PHITAB(INX)-PHITAB(INX-2))
            TRAN(MB1,J)=ZERO
            DO 20 I=1,MBIG
              INX=2*(I-J)
              TRAN(J,I)=(PHITAB(INX+2)-PHITAB(INX))+(PHITAB(INX+1)-
     1         PHITAB(INX-1))+(PHITAB(INX)-PHITAB(INX-2))
 20         CONTINUE
          ELSE
          TRAN(J,0) = PHITAB(-J)
          TRAN(0,J) = PHITAB(J) - PHITAB(J-1)
          TRAN(MB1,J) = ZERO
          TRAN(J,MB1) = ZERO
          DO 25 I = 1, MBIG
 25       TRAN(J,I) = PHITAB(I-J) - PHITAB(I-J-1)
          ENDIF
 30     CONTINUE
        TRAN(0,0)=PHITAB(0)
        IF (ISREAL) TRAN(0,0) = SIX * PHITAB(0)
        TRAN(MB1,MB1)=ONE
        TRAN(MB1,0)=ZERO
        TRAN(0,MB1) = ZERO
        DO 40 J=0,MB1
          COPY(J,MB1)=ONE
          DO 40 I=0,MBIG
          COPY(J,I)=-TRAN(J,I)
          IF (I.EQ.J) COPY(J,I)=COPY(J,I)+ONE
 40     CONTINUE
C
C     SOLVE THE EQUATIONS BY GAUSS-JORDAN ELIMINATION
C
        DO 70 J=0,MBIG
          PIVOT=COPY(J,J)
          DO 60 JJ=J+1,MBIG
            FMULT=COPY(JJ,J)/PIVOT
            DO 50 I=0,MB1
 50           COPY(JJ,I)=COPY(JJ,I)-FMULT*COPY(J,I)
 60       CONTINUE
 70     CONTINUE
        DO 90 J=MBIG,0,-1
          TOTAL=COPY(J,MB1)
          DO 80 I=J+1,MBIG
 80         TOTAL=TOTAL-ANSWER(I)*COPY(J,I)
          IF (COPY(J,J).LT.TOLER/EPS) IFAULT=64
          ANSWER(J)=TOTAL/COPY(J,J)
 90     CONTINUE
C
C     EXIT LOOP IF DATA ARE INTEGER
C
        IF (.NOT. ISREAL) THEN
           ACCE1 = ANSWER(0)
           ACCE2 = ANSWER((MBIG+1)/2)
           GO TO 120
           ENDIF
C
C     EXTRACT SOLUTIONS FOR CONVENTIONAL AND FIR CUSUM ARL'S
C
        RICHAR(MLOOP,1,1)=ANSWER(0)
        RICHAR(MLOOP,1,2)=(ANSWER(MBIG/2)+ANSWER(MBIG/2+1))/TWO
C
C     PERFORM RICHARDSON EXTRAPOLATION
C
        POW=ONE
        DO 100 JL=2,MLOOP
          POW=POW*FOUR
          DO 100 IW=1,2
          RICHAR(MLOOP,JL,IW)=(POW*RICHAR(MLOOP,JL-1,IW)-RICHAR(MLOOP-1,
     1     JL-1,IW))/(POW-1)
 100    CONTINUE
C       WRITE(*,'(7F11.4)') (RICHAR(MLOOP,JL,1), JL = 1, MLOOP)
        IF (MLOOP.EQ.1) GO TO 110
        OACCE1=RICHAR(MLOOP-1,MLOOP-1,1)
        OACCE2=RICHAR(MLOOP-1,MLOOP-1,2)
        ACCE1=RICHAR(MLOOP,MLOOP,1)
        ACCE2=RICHAR(MLOOP,MLOOP,2)
        ESTERR=MAX(ABS(ACCE1-OACCE1),ABS(ACCE2-OACCE2))/ABS(ACCE1)
C
C     TEST FOR CONVERGENCE
C
        IF (ESTERR.LT.EPS) GO TO 120
 110  CONTINUE
C
C     REQUESTED ACCURACY NOT ATTAINED
C
      IFAULT=IFAULT+4
 120  REGARL=ACCE1
      FIRARL=ACCE2
C
C     GET STEADY-STATE ARL
C
      DO 200 I = 0, MB1
      TRAN(I,MB1) = ONE
      STEADY(I) = ZERO
      DO 210 J = 0, MBIG
      TRAN(I,MB1) = TRAN(I,MB1) - TRAN(I,J)
 210  CONTINUE
 200  CONTINUE
      STEADY(0) = ONE
      OLDSTE = -1.E10
      SSARL =  1.E10
      ANSWER(MB1) = ZERO
      DO 150 ITER = 1, 200
      IF (ABS(OLDSTE-SSARL) .LT. EPS * ABS(SSARL)) GO TO 150
      OLDSTE = SSARL
      SSARL = ZERO
      DO 220 I = 0, MB1
      WORK(I) = STEADY(I)
      STEADY(I) = ZERO
 220  CONTINUE
      DO 230 I = 0, MB1
      DO 250 J = 0, MB1
      STEADY(I) = STEADY(I) + TRAN(J,I) * WORK(J)
 250  CONTINUE
      SSARL = SSARL + STEADY(I) * ANSWER(I)
 230  CONTINUE
      STEADY(0) = STEADY(0) + STEADY(MB1)
      STEADY(MB1) = ZERO
 150  CONTINUE
 130  CONTINUE
      RETURN
      END