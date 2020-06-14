C$PROG SSUM1     - Computes raw & net - sum, centroid, fwhm - for cuss1
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SSUM1(N1,N2,XL,YL,YR,SRAW,SNET,CNR,CNN,FWR,FWN)
C   
C     ------------------------------------------------------------------
      COMMON/DML2/ IDATF(65536),IHEDF(32),MAXCH
      INTEGER*4    IDATF,       IHEDF,    MAXCH
C     ------------------------------------------------------------------
      REAL*8 SRAW,SNET,CRAW,CNET
C
      REAL*8 DENO,DYDX,XNOW,YNOW,SUMXXY,SUMXY,SUMY,ZUMXXY,ZUMXY,ZUMY
C   
      SAVE
C
C     ------------------------------------------------------------------
C     CALCULATES RAW & NET - SUM, CENTROID & FWHM
C     ------------------------------------------------------------------
C   
      DENO=N2-N1
      IF(DENO.LT.1.0) DENO=1.0
      DYDX=(YR-YL)/DENO
      XNOW=0.0
      YNOW=YL
      SUMXXY=0.0
      SUMXY=0.0
      SUMY=0.0
      ZUMXXY=0.0
      ZUMXY=0.0
      ZUMY=0.0
C   
      DO 10 I=N1,N2
      IDAT=IDATF(I)
      CRAW=FLOAT(IDAT)
      CNET=CRAW-YNOW
      SUMXXY=SUMXXY+XNOW*XNOW*CNET
      SUMXY=SUMXY+XNOW*CNET
      SUMY=SUMY+CNET
      ZUMXXY=ZUMXXY+XNOW*XNOW*CRAW
      ZUMXY=ZUMXY+XNOW*CRAW
      ZUMY=ZUMY+CRAW
      XNOW=XNOW+1.0
      YNOW=YNOW+DYDX
   10 CONTINUE
C   
      IF(ZUMY.LT.1.0) ZUMY=1.0
      CNR=ZUMXY/ZUMY
      SIGSQ=ZUMXXY/ZUMY-CNR*CNR
      IF(SIGSQ.LT.0.0001) SIGSQ=0.0001
      FWR=2.354*SQRT(SIGSQ)
      CNR=CNR+XL
C   
      IF(SUMY.LT.1.0) SUMY=1.0
      CNN=SUMXY/SUMY
      SIGSQ=SUMXXY/SUMY-CNN*CNN
      IF(SIGSQ.LT.0.0001) SIGSQ=0.0001
      FWN=2.354*SQRT(SIGSQ)
      CNN=CNN+XL
C   
      SNET=SUMY
      SRAW=ZUMY
C   
      RETURN
      END
