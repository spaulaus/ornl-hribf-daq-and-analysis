C$PROG CMPSPLIN  - Command processor for Spline Fit operations
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 07/01/2003
C     ******************************************************************
C
      SUBROUTINE CMPSPLIN(IDONE,IERR)
C
      IMPLICIT NONE 
C
      INTEGER*4  MXDAT
C
      PARAMETER (MXDAT=500)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/FT02/ LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
      COMMON/FT03/ XIN(MXDAT),YIN(MXDAT),UIN(MXDAT),NDAT
      REAL*8       XIN,       YIN,       UIN
      INTEGER*4                                     NDAT
C     ------------------------------------------------------------------
      COMMON/FT05/ LWD4(4,20)
      INTEGER*4    LWD4
C     ------------------------------------------------------------------
      COMMON/FT08/ NAMF(20)
      INTEGER*4    NAMF
C
      CHARACTER*80 CNAMF
      EQUIVALENCE (CNAMF,NAMF)
C     ------------------------------------------------------------------
      COMMON/FT10/ KINDUU,KINDU,USUN,ALUN,UMUL
      INTEGER*4    KINDUU,KINDU
      REAL*8                    USUN,ALUN,UMUL
C     ------------------------------------------------------------------
      COMMON/FT12/ TITLA,TITLB,TITLC
      CHARACTER*80 TITLA,TITLB,TITLC
C     ------------------------------------------------------------------
      CHARACTER*4  IDONE
C
      REAL*4       XLIN(MXDAT),YLIN(MXDAT)
C
      REAL*4       YS(MXDAT),YSP(MXDAT),TEMP(MXDAT,9)
C
      REAL*4       X(MXDAT),Y(MXDAT),D(MXDAT),S,EPS,SIGMA
C
      REAL*8       YCAL(MXDAT),YY
C
      REAL*4       XV,XLO,XHI,YINT,PCD
C
      INTEGER*4    IV,JV,KV
C
      INTEGER*4    ISW,NLIN,LENG,STRLEN
C
      INTEGER*4    IERR,IDN,KIND,NLN,II,I
C
      REAL*4       QFN,TX,DX,YIV,CURV2
C
      REAL*4       FITS,FITEPS,FITSIG
      DATA         FITS,FITEPS,FITSIG/-1.0,-1.0,-1.0/
C
      CHARACTER*80 FITFIL
      DATA         FITFIL/'????'/
C
      CHARACTER*4  FITDUN
      DATA         FITDUN/'NO  '/
C
      CHARACTER*4  KMD
      EQUIVALENCE (KMD,LWD(1,1))
C
      INTEGER*4    ITPF
      DATA         ITPF/Z'0C202020'/
C
      REAL*4       SSAV,EPSSAV,SIGSAV
      DATA         SSAV,EPSSAV,SIGSAV/-1.0,-1.0,-1.0/
C     ------------------------------------------------------------------
C
      IDONE='NO  '
C
      IERR=0
C
      CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
C
C
      IF(KMD.EQ.'S   ') GO TO 100
      IF(KMD.EQ.'EPS ') GO TO 110
      IF(KMD.EQ.'SIG ') GO TO 120
      IF(KMD.EQ.'SSTA') GO TO 130
      IF(KMD.EQ.'SVAL') GO TO 150
      IF(KMD.EQ.'SFIT') GO TO 300
C
      GO TO 2510
C
  100 IF(XV.LT.0.0) GO TO 2030
      SSAV=-1.0
      IF(NF.EQ.2) SSAV=XV
      GO TO 2500
C
  110 IF(XV.LT.0.0) GO TO 2040
      IF(XV.GT.1.0) GO TO 2040
      EPSSAV=-1.0
      IF(NF.EQ.2) EPSSAV=XV
      GO TO 2500
C
  120 IF(XV.LT.0.0) GO TO 2050
      SIGSAV=-1.0
      IF(NF.EQ.2) SIGSAV=XV
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Display/Log status information
C     ------------------------------------------------------------------
C
  130 WRITE(CMSSG,135)SSAV,EPSSAV,SIGSAV
  135 FORMAT(' S,EPS,SIG =',3F12.4)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Display/Log interpolated values or tables
C     ------------------------------------------------------------------
C
  150 IF(FITDUN.NE.'YES ') GO TO 2020
      IF(NF.EQ.4) GO TO 200
C
      YINT=CURV2(XV,NDAT,X,YS,YSP,SIGMA)
C
      WRITE(CMSSG,155)XV,YINT
  155 FORMAT('X,Y =',2F10.3)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2500
C
  200 CALL MILV(LWD(1,3),IV,XHI,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
C
      CALL MILV(LWD(1,4),IV,DX,KIND,IERR)
      IF(IERR.NE.0) GO TO 2010
C
      WRITE(LOGUT,210)
      WRITE(LOGUP,210)
  210 FORMAT(' =================================================')
C
      WRITE(LOGUT,215)
      WRITE(LOGUP,215)
  215 FORMAT(' Spline Interpolation')
C
      WRITE(LOGUT,210)
      WRITE(LOGUP,210)
C
      WRITE(LOGUT,220)FITFIL(1:STRLEN(FITFIL))
      WRITE(LOGUP,220)FITFIL(1:STRLEN(FITFIL))
  220 FORMAT(' File: ',A)
C
      WRITE(LOGUT,210)
      WRITE(LOGUP,210)
C
      WRITE(LOGUT,225)
      WRITE(LOGUP,225)
  225 FORMAT('   IDN           S         EPS       SIGMA     QFN')
C
      WRITE(LOGUT,230)IDN,S,EPS,SIGMA,QFN
      WRITE(LOGUP,230)IDN,S,EPS,SIGMA,QFN
  230 FORMAT(I6,3F12.3,F8.3)
C
      WRITE(LOGUT,210)
      WRITE(LOGUP,210)
C
      WRITE(LOGUT,235)
      WRITE(LOGUP,235)
  235 FORMAT('     I           X           Y')
C
      TX=XV
C
      I=0
C
  250 I=I+1
      XLIN(I)=TX
      YLIN(I)=CURV2(TX,NDAT,X,YS,YSP,SIGMA)
      WRITE(LOGUT,255)I,XLIN(I),YLIN(I)
      WRITE(LOGUP,255)I,XLIN(I),YLIN(I)
  255 FORMAT(I6,1P2E12.3)
      TX=TX+DX
      IF(TX.LE.XHI) GO TO 250
      NLIN=I
      CALL PLOTFIT(3,XIN,YIN,YCAL,NDAT,XLIN,YLIN,NLIN)
      GO TO 2500
C
C     ------------------------------------------------------------------
C     Do a Spline fit
C
C     FIT REQUEST - IS NEXT FIELD NUMERIC INDICATING DATA SET #
C     ------------------------------------------------------------------
C
  300 IDN=IV
C
      CALL GREAD(IWD,LWD4,ITYP,NF,'SETW',16,NTER)
C
      CALL GETDATX(IDN,IERR)
C
      CALL GREAD(IWD,LWD4,ITYP,NF,'SETW', 8,NTER)
C
      IF(IERR.NE.0) GO TO 2060
C
C     ------------------------------------------------------------------
C     COMPUTE THE UNCERTAINTIES U(I) BY REQUIRED METHOD
C
C     KINDUU= 1 SAYS ALL UNCERT = ALUN (%)
C     KINDUU= 2 SAYS UNSPECIFIED UNCERT = USUN (%)
C     KINDU = 1 SAYS COMPUTE UNCERT AS COUNTING STATISTICS
C     KINDU = 2 SAYS UNCERT GIVEN BY "UIN" IS ABSOLUTE
C     KINDU = 3 SAYS UNCERT GIVEN BY "UIN" IS IN %
C     ------------------------------------------------------------------
C
  320 IF(KINDUU.EQ.1) GO TO 350
      IF(KINDU.EQ.1)  GO TO 370
      GO TO 385
  350 DO 360 I=1,NDAT
      D(I)=0.01*ALUN*YIN(I)
  360 CONTINUE
      GO TO 440
  370 DO 380 I=1,NDAT
      YY=YIN(I)
      IF(YY.LT.1.0) YY=1.0
      D(I)=DSQRT(YY)
  380 CONTINUE
      GO TO 440
  385 DO 400 I=1,NDAT
      IF(UIN(I).EQ.0.0) GO TO 390
      IF(KINDU.EQ.2) D(I)=UIN(I)*UMUL
      IF(KINDU.EQ.3) D(I)=0.01*UIN(I)*YIN(I)*UMUL
      GO TO 400
  390 D(I)=0.01*USUN*YIN(I)
  400 CONTINUE
C
C
C     ------------------------------------------------------------------
C     Display/log the results
C     ------------------------------------------------------------------
C
  440 DO 520 I=1,NDAT
      X(I)=XIN(I)
      Y(I)=YIN(I)
  520 CONTINUE
C
      ISW=0
      S=FLOAT(NDAT)
      EPS=SQRT(2.0/S)
      SIGMA=1.0
C
      IF(SSAV.GE.0.0)   S=SSAV
      IF(EPSSAV.GE.0.0) EPS=EPSSAV
      IF(SIGSAV.GE.0.0) SIGMA=SIGSAV
C
      CALL CURVS(NDAT,
     &              X,
     &              Y,
     &              D,
     &            ISW,
     &              S,
     &            EPS,
     &             YS,
     &            YSP,
     &          SIGMA,
     &           TEMP,
     &           IERR)
C
      IF(IERR.NE.0) THEN
      CALL SPLINERR(IERR)
      FITDUN='NO  '
      GO TO 2500
      ENDIF
C
      FITDUN='YES '
      FITFIL=CNAMF
      FITS  =S
      FITEPS=EPS
      FITSIG=SIGMA
C
      QFN=0.0
C
      DO 530 I=1,NDAT
      QFN=QFN+(Y(I)-YS(I))**2/(D(I)*D(I))
  530 CONTINUE
      QFN=QFN/FLOAT(NDAT)
C
      WRITE(LOGUT,210)
      WRITE(LOGUP,210)
C
      WRITE(LOGUT,535)
      WRITE(LOGUP,535)
  535 FORMAT(' Spline Fit')
C
      WRITE(LOGUT,210)
      WRITE(LOGUP,210)
C
      WRITE(LOGUT,220)CNAMF(1:STRLEN(CNAMF))
      WRITE(LOGUP,220)CNAMF(1:STRLEN(CNAMF))
C
      WRITE(LOGUT,210)
      WRITE(LOGUP,210)
C
      WRITE(LOGUT,225)
      WRITE(LOGUP,225)
C
      WRITE(LOGUT,230)IDN,S,EPS,SIGMA,QFN
      WRITE(LOGUP,230)IDN,S,EPS,SIGMA,QFN
C
      WRITE(LOGUT,210)
      WRITE(LOGUP,210)
C
      WRITE(LOGUT,540)
      WRITE(LOGUP,540)
  540 FORMAT('     I           X           Y        YCAL    %DIF')
C
      DO 550 I=1,NDAT
      XLIN(I)=XIN(I)
      YLIN(I)=YS(I)
      YCAL(I) =YS(I)
      PCD=100.0*(YS(I)-Y(I))/YS(I)
      WRITE(LOGUT,545)I,X(I),Y(I),YS(I),PCD
      WRITE(LOGUP,545)I,X(I),Y(I),YS(I),PCD
  545 FORMAT(I6,1P3E12.3,0PF8.2)
  550 CONTINUE
C
      NLIN=NDAT
C
      LENG=STRLEN(CNAMF)
C
      IF(LENG.GT.20) LENG=20
C
      TITLC=' '
C
      WRITE(TITLC,600)CNAMF(1:LENG),IDN,S,EPS,SIGMA
C
  600 FORMAT('Spline_fit file=',A,'  ID,S,EPS,SIG=',I3,3F8.3)
C
      TX=XIN(1)
      DX=(XIN(NDAT)-XIN(1))/100.0
      NLIN=101
C
      DO 650 I=1,101
      XLIN(I)=TX
      YLIN(I)=CURV2(TX,NDAT,X,YS,YSP,SIGMA)
      TX=TX+DX
  650 CONTINUE
C
      CALL PLOTFIT(3,XIN,YIN,YCAL,NDAT,XLIN,YLIN,NLIN)
C
      GO TO 2500
C
C     ------------------------------------------------------------------
C     LIST DIAGNOSTIC MESSAGES
C     ------------------------------------------------------------------
C
 2010 WRITE(CMSSG,2015)
 2015 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE')
      GO TO 2400
C
 2020 WRITE(CMSSG,2025)
 2025 FORMAT('No valid spline fit available for interpolation')
      GO TO 2400
C
 2030 WRITE(CMSSG,2035)
 2035 FORMAT('S must be a positive number - Command Ignored')
      GO TO 2400
C
 2040 WRITE(CMSSG,2045)
 2045 FORMAT('EPS must be GE.0.0 and LE.1.0  - Command Ignored')
      GO TO 2400
C
 2050 WRITE(CMSSG,2055)
 2055 FORMAT('SIGMA must be a positive number - Command Ignored')
      GO TO 2400
C
 2060 WRITE(CMSSG,2065)IDN
 2065 FORMAT('Requested ID =',I6,'  not found - Command Ignored')
      GO TO 2400
C
 2400 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
C
 2500 IDONE='YES '
 2510 RETURN
      END
