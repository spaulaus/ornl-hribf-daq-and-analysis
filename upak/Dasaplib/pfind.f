C$PROG PFIND
      SUBROUTINE PFIND(DATA,LOCA,N1,N2,IFWHM,SIGMA,MAXPK,NPK)
C   
      INTEGER*4 DATA(*),CTR,WINGS
C
      REAL*4    LOCA(*)
C
      SAVE
C   
C     ****************************************************************
C     PEAK-FINDER ROUTINE    ORNL 5/4/76 JDL.
C   
C     N1      = CHANNEL # AT START OF SCAN
C     N2      = CHANNEL # AT STOP  OF SCAN
C     IFWHM   = FWHM ESTIMATE SUPPLIED BY CALLING PROG
C     SIGMA   = PEAK DETECTION THRESHOLD (STD DEV ABOVE BKG)
C     DATA(I) = DATA ARRAY TO BE SCANNED
C     ****************************************************************
C   
      NPK=0
      NP=0
      LOCK=0
      N=0
      P1=0.0
      P2=0.0
      P3=0.0
      PEAK1=0.0
      PEAK2=0.0
      PEAK3=0.0
      SUM=0.0
      SUM2=0.0
C   
      IF(IFWHM.LT.1) IFWHM=1
      J1=-(2*IFWHM-1)/4
      J2=J1+IFWHM-1
      JODD=1-(IFWHM-2*(IFWHM/2))
C   
      ND1=N1+IFWHM
      ND2=N2-IFWHM-1
C   
C     ****************************************************************
C     TST FOR ANY NEGATIVE, IF YES ADD TO DATA TO MAKE ALL POSITIVE
C     ****************************************************************
C   
      MINY=0
      IADD=0
      DO 10 I=N1,N2
      IF(DATA(I).LT.MINY) MINY=DATA(I)
   10 CONTINUE
      IF(MINY.GE.0) GO TO 20
      IADD=-MINY
      DO 15 I=N1,N2
      DATA(I)=DATA(I)+IADD
   15 CONTINUE
C   
C     ****************************************************************
C     NOW GO AHEAD AND DO THE PEAKFIND BIT
C     ****************************************************************
C   
   20 DO 100 ND=ND1,ND2
C   
C     SUM COUNTS IN CENTER AND WINGS OF DIFFERENTIAL FUNCTION
C   
      CTR=0
      WINGS=0
C   
      DO 30 J=J1,J2
      JD=ND+J
      CTR=CTR+DATA(JD)
      IF(J.LE.0) GO TO 30
      JL=ND-J+J1
      JR=JD+J2
      WINGS=WINGS+DATA(JL)+DATA(JR)
   30 CONTINUE
C   
C     IF IFWHM IS ODD, AVERAGE DATA IN SPLIT CELLS AT ENDS
C   
      W4=0.0
      IF(JODD.NE.0) GO TO 40
      JL=ND+J1+J1
      JR=ND+J2+J2
      W4=0.25*FLOAT(DATA(JL-1)+DATA(JL)+DATA(JR)+DATA(JR+1))
C   
C     COMPUTE HEIGHT OF SECOND DERIVATIVE (NEG) RELATIVE TO NOISE
C   
   40 CONTINUE
      S2=SUM
      SUM=FLOAT(CTR-WINGS)-W4
      ROOT=SQRT(FLOAT(CTR+WINGS+1)+W4)
      P1=P2
      P2=P3
      DENO=ROOT
      IF(DENO.LT.1.0E-6) DENO=1.0E-6
      P3=SUM/DENO
      IF(LOCK.EQ.0) GO TO 90
      IF(P2.LE.PEAK2.OR.P3.GE.P2) GO TO 60
C   
C     SAVE THREE VALUES AT RELATIVE MAXIMUM
C   
      PEAK1=P1
      PEAK2=P2
      PEAK3=P3
      SUM2=S2
      NP=ND-1
   60 CONTINUE
      IF(P3.GE.SIGMA) GO TO 100
C   
C     ESTIMATE LOCATION AND CRUDE SIZE OF PEAK
C   
      LOCK=0
      DENO=PEAK1-PEAK2+PEAK3-PEAK2
      IF(ABS(DENO).LT.1.0E-6) DENO=1.0E-6
      PC=0.5*(PEAK1-PEAK3)/DENO
      CHAN=FLOAT(NP)+PC+0.5*FLOAT(JODD)
      DENO=PEAK3-PEAK1
      IF(ABS(DENO).LT.1.0E-6) DENO=1.0E-6
      PW=4.0*PC/DENO
      DENO=PW
      IF(ABS(DENO).LT.1.0E-6) DENO=1.0E-6
      PKSIG=PC*PC/DENO+PEAK2
      N=N+1
      IF(N.GT.MAXPK) N=MAXPK
      SIZE=SUM2+SUM2
      PEAK2=0.0
      LOCA(N)=CHAN
C   
   90 CONTINUE
      IF(P3.GE.SIGMA) LOCK=1
  100 CONTINUE
      NPK=N
      LOCA(NPK+1)=1000000.0
C   
C   
C     ****************************************************************
C     TST FOR OFFSET ADDED - I.E. IS DATA RESTORE REQUIRED
C     ****************************************************************
C   
      IF(IADD.EQ.0) RETURN
C   
      DO 110 I=N1,N2
      DATA(I)=DATA(I)-IADD
  110 CONTINUE
      RETURN
      END
