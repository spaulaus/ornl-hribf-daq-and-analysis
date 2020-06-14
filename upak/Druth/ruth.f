C$PROG RUTH      - Rutherford Cross Section program
C
C     ******************************************************************
C     RUTHERFORD CROSS SECTION     MLH, AUG 1, 1973   JAN. 21, 1974.
C     ******************************************************************
C     REVISED 6-25-82 FOR PERKIN-ELMER COMPUTER
C     INSTRUCTIONS TO USER ON UNIT 6 (VDT)
C     DATA INPUT FROM UNIT 5         (VDT)
C     OUTPUT TABLE ON UNIT 7         (ruth.log)
C     ------------------------------------------------------------------
C     4-16-86  REVISED AGAIN FOR MORE FLEXIBILITY IN INPUT
C              ALLOW ANY INCREMENT IN ANGLE
C              ALLOW MIN, MAX ANGLES TO BE SPECIFIED
C     2-01-91  SUPPLY DEFAULT PSIMAX=180.; WAS OMITTED BEFORE
C     ------------------------------------------------------------------
C     FORMULAS FROM J. MARION'S 1960 TABLES, P. 163,179.
C     SEE NPA207,38 FOR SRATIO.
C     ------------------------------------------------------------------
C   
      CHARACTER*36 MESS1,MESS2,MESS3,MESS4,MESS5,
     &             MESS6,MESS7,MESS8,MESS9,MESS0
C
      DATA MESS1/'                       Projectile A:'/
      DATA MESS2/'                       Projectile Z:'/
      DATA MESS3/'                           Target A:'/
      DATA MESS4/'                           Target Z:'/
      DATA MESS5/'                 Beam energy in MeV:'/
      DATA MESS6/'                     Starting angle:'/
      DATA MESS7/'  Angle increment (default=1.0 deg):'/
      DATA MESS8/'  Maximum angle (default=180.0 deg):'/
      DATA MESS9/'  OPTIONAL:                 Give d0:'/
      DATA MESS0/'                         Give delta:'/
C     ------------------------------------------------------------------
C
      OPEN(UNIT       = 7,
     &     FILE       = 'ruth.log',
     &     STATUS     = 'UNKNOWN')
C
c     CLOSE(UNIT=7,DISP='DELETE')
      CLOSE(UNIT=7)
C
      OPEN(UNIT       = 7,
     &     FILE       = 'ruth.log',
     &     STATUS     = 'REPLACE')
C   
      WRITE(6,10)MESS1
   10 FORMAT(1H ,A,$)
      READ(5,15)A1
   15 FORMAT(F10.0)
C
      WRITE(6,10)MESS2
      READ(5,15) Z1
C
      WRITE(6,10)MESS3
      READ(5,15) A2
C
      WRITE(6,10)MESS4
      READ(5,15) Z2
C
      WRITE(6,10)MESS5
      READ(5,15) BEAM
C
      WRITE(6,10)MESS6
      READ(5,15) PSIZRO
C
      WRITE(6,10)MESS7
      READ(5,15) DELPSI
      IF(DELPSI.EQ.0.0) DELPSI=1.0
C
      WRITE(6,10)MESS8
      READ(5,15) PSIMAX
      IF(PSIMAX.EQ.0.0) PSIMAX=180.0
C
      WRITE(6,10)MESS9
      READ(5,15) D0
      IF(D0.EQ.0.0) GO TO 40
C
      WRITE(6,10)MESS0
      READ(5,15) DELTA
      GO TO 50
    
   40 D0=1.68
      DELTA=0.55
C
   50 SUMA=  A1**(1./3.)+A2**(1./3.)
      DD=D0*SUMA
C
      CONST=0.720*Z1*Z2/(BEAM*A2/(A1+A2))
C
      WRITE(7,60)A1,Z1
      WRITE(7,61)A2,Z2
      WRITE(7,62)BEAM
      WRITE(7,63)SUMA
      WRITE(7,64)D0,DELTA
C
C     ------------------------------------------------------------------
   60 FORMAT(1H ,'RUTHERFORD C.S. (MB/SR) FOR A1,Z1= ',2F7.3,
     &           ' PROJECTILE')
   61 FORMAT(1H ,'SCATTERED BY                A2,Z2= ',2F7.3,' TARGET')
   62 FORMAT(1H ,'EBEAM (MEV) (NONRELATIVISTIC)    = ',F7.3)
   63 FORMAT(1H ,'D=CLASSICAL DISTANCE OF CLOSEST APPROACH (10**-13 CM)'
     &            ,1X,'A1**1/3+A2**1/3 =',F8.4)
   64 FORMAT(1H ,'RATIO TO RUTHERFORD ESTIMATED FROM NUC PHYS A207,
     & 38, WITH SMALL D0=',F4.2,', DELTA=',F4.2)
C
      WRITE(7,70)
   70 FORMAT(1H )
      WRITE(7,80)
   80 FORMAT(1H ,65X,'ESTIMATED'/
     &        'ANG LAB',5X,'SIGMA LAB',3X,'ANG CM',6X,'SIGMA CM',
     &  5X,'D(FM)',3X,'R0(FM)',3X,'SIG/SIGR',2X,'ANG LAB',3X,'ESCATT')
C
      RAD=57.2957795
      IMAX=PSIMAX/DELPSI + 1.0
C
      NLN=0
C
      DO 160 I=1,IMAX
C
      PSI=FLOAT(I-1)*DELPSI+PSIZRO
      IF(PSI.GT.PSIMAX) GO TO 200
C
      PSIR=PSI/RAD
      R=SIN(PSIR)*A1/A2
C
      IF(R.EQ.0.0) GO TO 160
C
      IF(R.LT.-1.0.OR.R.GT.1.0) GO TO 200
C
      THETR=PSIR+ARCSIN(R)
      THET=RAD*THETR
      SIGCM=1.296*(Z1*Z2/BEAM)**2*((A1+A2)/A2)**2/(SIN(THETR/2.0))**4
      SIGLAB=SIGCM*(SIN(THETR))**2/(COS(THETR-PSIR)*(SIN(PSIR))**2)
      ESCATT=BEAM*(1.0-2.0*A1*A2/(A1+A2)**2 *(1.0-COS(THETR)))
      D=CONST*(1.0+1.0/SIN(THETR/2.0))
      DA=D/SUMA
      DIFF=D-DD
C
      IF(DIFF) 100,90,90
C
   90 SRATIO=1.0
      GO TO 110
C
  100 SRATIO=EXP(DIFF/DELTA)
C
  110 K=1
C
      IF(SIGCM .GE.1.0E6.OR.SIGLAB.GE.1.0E6) K=2
      IF(SIGCM .LT.1.E-2.OR.SIGLAB.LT.1.E-2) K=2
C
      GO TO (120,140), K
C
  120 WRITE(7,130) PSI,SIGLAB,THET,SIGCM,D,DA,SRATIO,PSI,ESCATT
  130 FORMAT(  F7.2,F14.5,F9.2,F14.5,F10.2,F9.3,G13.5,F7.2,F9.3)
      NLN=NLN+1
      IF(NLN.GE.5) THEN
      WRITE(7,205)
      NLN=0
      ENDIF
      GO TO 160
C
  140 WRITE(7,150) PSI,SIGLAB,THET,SIGCM,D,DA,SRATIO,PSI,ESCATT
  150 FORMAT(  F7.2,E14.5,F9.2,E14.5,F10.2,F9.3,G13.5,F7.2,F9.3)
      NLN=NLN+1
      IF(NLN.GE.5) THEN
      WRITE(7,205)
      NLN=0
      ENDIF
C
  160 CONTINUE
C
  200 WRITE(6,205)
  205 FORMAT(1H )
      WRITE(6,210)
  210 FORMAT(1H ,'Output is written to ruth.log')
      CALL EXIT(0)
      END
C$PROG ARCSIN
C
      FUNCTION ARCSIN(R)
C
      DENOM=1.0-R**2
C
      IF(DENOM) 1,1,2
C
    1 ARCSIN=(3.141592654/2.0)*SIGN(1.0,R)
C
      RETURN
C
    2 ARCSIN=ATAN2(R,SQRT(DENOM))
C
      RETURN
      END
