      subroutine getden(ref,di,denrat,echo,change)
      implicit double precision (a-h,o-z)
      logical echo,change
      data half/0.5d0/,p01/0.01d0/,one/1.d0/
      errmax = one
      change = .false.
      do 30 denrat = 1.d0, 64.01d0
      refden = ref * denrat
      diden  = di  * denrat
      if (diden .gt. 128) go to 30
      error = abs(mod(refden+p01,one) - p01) +
     1    abs(mod(diden+p01,one) - p01)
      if (error .lt. p01) go to 40
      if (error .lt. errmax) then
        errmax = error
        useden = denrat
        useref = int(refden + half) / denrat
        usedi  = int(diden  + half) / denrat
        endif
 30   continue
      if (echo) write(*,101) int(useden)
 101  format(' Choices not rational with small enough denominator'/
     1 ' They will be edited to the closest manageable denominator',i4)
      ref = useref
      di  = usedi
      denrat = useden
 40   continue
      return
      end
      subroutine calcus(di,ref,ndis,denrat,arg2,wl,wu,
     1 regarl,firarl,ssarl,eps,esterr,ifault)
      implicit double precision (a-h,o-z)
      external normal,varup,vardn,poiup,poidn,binup,bindn,
     1 nbinup,nbindn,gauiup,gauidn
      go to (310,320,330,340,350,360,370,380,390,400,410), ndis
 310  call cusrlg(di, ref,normal,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 320  call cusrlg(di, ref,varup ,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 330  call cusrlg(di, ref,vardn ,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 340  call cusrlg(di, ref,poiup ,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 350  call cusrlg(di, ref,poidn ,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 360  call cusrlg(di, ref,binup ,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 370  call cusrlg(di, ref,bindn ,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 380  call cusrlg(di, ref,nbinup,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 390  call cusrlg(di, ref,nbindn,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 400  call cusrlg(di, ref,gauiup,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 410  call cusrlg(di, ref,gauidn,denrat,arg2,wl,wu,regarl,
     1 firarl,ssarl,eps,esterr,ifault)
      go to 500
 500  continue
      return
      end
      double precision function normal(arg,arg2)
      implicit double precision (a-h,o-z)
      cumdis = phi(arg)
      normal = cumdis
      return
      end
      double precision function varup(arg,arg2)
      implicit double precision (a-h,o-z)
      common /parms/ realno, ndf
      df = ndf
      cumdis = 0
      if (arg .lt. 0) go to 10
      arg3 = arg * df
      cumdis = 1 - chisq(arg3,ndf)
 10   continue
      varup = cumdis
      return
      end
      double precision function vardn(arg,arg2)
      implicit double precision (a-h,o-z)
      common /parms/ realno, ndf
      df = ndf
      cumdis = 1
      if (arg .gt. 0) go to 10
      arg3 = -arg * df
      cumdis = chisq(arg3,ndf)
 10   continue
      vardn = cumdis
      return
      end
      double precision function poiup(arg,arg2)
      implicit double precision (a-h,o-z)
      cumdis = 0
      if (arg .lt. 0) go to 10
      inx = arg + .5
      t0 = exp(-arg2)
      do 1 i = 0, inx
      cumdis = cumdis + t0
      t0 = t0 * arg2 / (i+1)
 1    continue
 10   continue
      poiup = cumdis
      return
      end
      double precision function poidn(arg,arg2)
      implicit double precision (a-h,o-z)
      cumdis = 1
      if (arg .ge. 0) go to 10
      aarg = abs(arg)
      inx = aarg - .5
      t0 = exp(-arg2)
      do 1 i = 0, inx
      cumdis = cumdis - t0
      t0 = t0 * arg2 / (i+1)
 1    continue
 10   continue
      poidn = cumdis
      return
      end
      double precision function binup(arg,arg2)
      implicit double precision (a-h,o-z)
      common /parms/ realno, nbig
      data one/1.d0/
      bign = nbig
      cumdis = 0
      if (arg .lt. 0) go to 10
      inx = arg + .5
      t0 = (one - arg2) ** nbig
      do 1 i = 0, inx
      fi = i
      cumdis = cumdis + t0
      t0 = t0 * (bign - fi) * arg2 / ((fi + one ) * (one - arg2))
 1    continue
 10   continue
      binup = cumdis
      return
      end
      double precision function bindn(arg,arg2)
      implicit double precision (a-h,o-z)
      common /parms/ realno, nbig
      data one /1.d0/
      bign = nbig
      cumdis = 1
      if (arg .ge. 0) go to 10
      aarg = abs(arg)
      inx = aarg - .5
      t0 = (one - arg2) ** nbig
      do 1 i = 0, inx
      fi = i
      cumdis = cumdis - t0
      t0 = t0 * (bign - fi) * arg2 / ((fi + one) * (one - arg2))
 1    continue
 10   continue
      bindn = cumdis
      return
      end
      double precision function nbinup(arg,c)
      implicit double precision (a-h,o-z)
      common /parms/ r, integ
      data zero/0.d0/, one/1.d0/
      cumdis = 0
      if (arg .lt. 0) go to 10
      inx = arg + .5
      t0 = (c / (one + c)) ** r
      cumdis = zero
      do 1 i = 0, inx
      fi = i
      cumdis = cumdis + t0
      t0 = t0 * (r + fi) / ((fi + one) * (one + c))
 1    continue
 10   continue
      nbinup = cumdis
      return
      end
      double precision function nbindn(arg,c)
      implicit double precision (a-h,o-z)
      common /parms/ r, integ
      data one /1.d0/
      cumdis = 1
      if (arg .ge. 0) go to 10
      aarg = abs(arg)
      inx = aarg - .5
      t0 = (c / (one + c)) ** r
      do 1 i = 0, inx
      fi = i
      cumdis = cumdis - t0
      t0 = t0 * (r + fi) / ((fi + one) * (one + c))
 1    continue
 10   continue
      nbindn = cumdis
      return
      end
      double precision function gauiup(arg, amu)
      implicit double precision (a-h,o-z)
      common /parms/ alam, integ
      data zero/0.d0/, one/1.d0/, two/2.d0/
      gauiup = zero
      if (arg .le. zero) return
      ruter = sqrt(alam / arg)
      tbymu = arg / amu
      gauiup = one - phi(ruter * (one - tbymu)) + exp(two * alam / amu)
     1 * phi(-ruter * (one + tbymu))
      return
      end
      double precision function gauidn(arg,amu)
      implicit double precision (a-h,o-z)
      common /parms/ alam, integ
      data zero/0.d0/, one/1.d0/, two/2.d0/
      gauidn = one
      if (arg .ge. zero) return
      ruter = sqrt(-alam / arg)
      tbymu = -arg / amu
      gauidn =  phi(ruter * (one - tbymu)) - exp(two * alam / amu)
     1 * phi(-ruter * (one + tbymu))
      return
      end
      function phi(x)
c
c     Algorithm for the normal integral
c     adapted from Applied Statistics algorithm 66
c
      implicit double precision(a-h,o-z)
      double precision ltone,utzero,zero,half,one,con,z,y,x
      logical up
      data ltone/8.5d0/, utzero/13.d0/
      data zero,half,one,con/0.d0,0.5d0,1.d0,1.28d0/
      up = .false.
      z = x
      if (z .ge. zero) go to 10
      up = .true.
      z = -z
 10   if (z .le. ltone .or. up .and. z .le. utzero) go to 20
      phi = zero
      go to 40
 20   y = half * z * z
      if (z .gt. con) go to 30
      phi = half - z * (0.398942280444d0 - 0.399903438504d0 * y /
     1 (y + 5.75885480458d0 - 29.8213557808d0 /
     2 (y + 2.62433121679d0 + 48.6959930692d0 /
     3 (y + 5.92885724438d0))))
      go to 40
 30   phi = 0.398942280385d0 * exp(-y) /
     1 (z - 3.8052d-8 + 1.00000615302d0 /
     2 (z + 3.98064794d-4 + 1.98615381364d0 /
     3 (z - 0.151679116635d0 + 5.29330324926d0 /
     4 (z + 4.8385912808d0 - 15.1508972451d0 /
     5 (z + 0.742380924027d0 + 30.789933034d0 /
     6 (z + 3.99019417011d0))))))
 40   if (.not. up) phi = one - phi
      return
      end
      function chisq(x,idf)
      implicit double precision (a-h,o-z)
c     evaluates the right tail area of the chi-squared distribution
c     with idf degrees of freedom, evaluated at x.  the method is
c     recursive.
      data one/1.d0/, tiny/1.d-12/, two/2.d0/, half/0.5d0/,
     1 terdiv/1.2533141373155d0/
      if (x .gt. tiny) go to 10
      chisq = one
      return
 10   continue
      low = mod(idf+1,2) + 1
      if (low .eq. 2) go to 1
      rutx = dsqrt (x)
      chisq = two * phi (-rutx)
      if (idf .eq. 1) return
      term = rutx * exp(-half * x) / terdiv
      fi = one
      go to 3
 1    continue
      chisq = exp ( -half * x)
      if (idf .eq. 2) return
      fi = two
      term = chisq * x * half
 3    continue
      low = low + 2
      do 4 i = low, idf, 2
      chisq = chisq + term
      fi = fi + 2
      term = term * x / fi
 4    continue
      return
      end
      SUBROUTINE CUSRLG (DI,REF,CUMDIS,DENRAT,ARG2,WINSRL,WINSRU,
     1 REGARL,FIRARL,SSARL,EPS,ESTERR,IFAULT)
C
C     Subroutine for the calculation of average run lengths of cusums
C
C     Douglas M Hawkins
C     Department of Applied Statistics
C     University of Minnesota
C
C     Code as of December 1993,  incorporates corrections to the code
C     Hawkins (1992) Communications in Statistics Simulation and
C     Computation, pages 1001-1020
C
      PARAMETER (MAXPOW=512,MAXHLF=9,TOLER=1.D-12)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TRAN(0:MAXPOW+1,0:MAXPOW+1), ANSWER(0:MAXPOW+1),
     1COPY(0:MAXPOW+1 ,0:MAXPOW+1), PHITAB(-2*MAXPOW-2:2*MAXPOW+2),
     2RICHAR(MAXHLF,MAXHLF,2), STEADY(0:MAXPOW+1), WORK(0:MAXPOW+1)
      LOGICAL ISREAL
      EXTERNAL CUMDIS
      DATA ZERO/0.D0/,HALF/0.5D0/,ONE/1.D0/,TWO/2.D0/,FOUR/4.D0/,FIVE/5.
     1D0/,SIX/6.D0/,P01/0.01D0/
c     write(*,*) 'di,ref,winsrl,winsru',di,ref,winsrl,winsru
      IFAULT=0
      REGARL=0
      FIRARL=0
      SSARL =0
      ESTERR = ONE
      DO 5 I = -2*MAXPOW-2, 2*MAXPOW+2
 5    PHITAB(I) = ZERO
C
C     Check parameters for feasibility
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
C     Set up coefficient matrix of linear equations
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
            END IF
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
c       write(*,'(i5,2f10.5)') i,arg,phitab(i)
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
C     Solve the equations by Gauss-Jordan elimination
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
C     Exit loop if data are integer
C
        IF (.NOT. ISREAL) THEN
           ACCE1 = ANSWER(0)
           ACCE2 = ANSWER((MBIG+1)/2)
           GO TO 120
           ENDIF
C
C     Extract solutions for conventional and FIR cusum ARL's
C
        RICHAR(MLOOP,1,1)=ANSWER(0)
        RICHAR(MLOOP,1,2)=(ANSWER(MBIG/2)+ANSWER(MBIG/2+1))/TWO
C
C     Perform Richardson extrapolation
C
        POW=ONE
        DO 100 JL=2,MLOOP
          POW=POW*FOUR
          DO 100 IW=1,2
          RICHAR(MLOOP,JL,IW)=(POW*RICHAR(MLOOP,JL-1,IW)-RICHAR(MLOOP-1,
     1     JL-1,IW))/(POW-1)
 100    CONTINUE
c       write(*,'(7f11.4)') (richar(mloop,jl,1), jl = 1, mloop)
        IF (MLOOP.EQ.1) GO TO 110
        OACCE1=RICHAR(MLOOP-1,MLOOP-1,1)
        OACCE2=RICHAR(MLOOP-1,MLOOP-1,2)
        ACCE1=RICHAR(MLOOP,MLOOP,1)
        ACCE2=RICHAR(MLOOP,MLOOP,2)
        ESTERR=MAX(ABS(ACCE1-OACCE1),ABS(ACCE2-OACCE2))/ABS(ACCE1)
C
C     Test for convergence
C
        IF (ESTERR.LT.EPS) GO TO 120
 110  CONTINUE
C
C     Requested accuracy not attained
C
      IFAULT=IFAULT+4
 120  REGARL=ACCE1
      FIRARL=ACCE2
c
c     GET STEADY-STATE ARL
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
