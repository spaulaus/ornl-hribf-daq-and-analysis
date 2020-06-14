C$PROG SSUM      - Computes raw & net - sum, centroid, fwhm - for CUSS
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SSUM(DAT,N1,N2,XL,YL,YR,SRAW,SNET,CNR,CNN,FWR,FWN)
C   
      DIMENSION DAT(*)
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
      ISUM=0
C   
      DO 10 I=N1,N2
      IDAT=DAT(I)+0.5
      ISUM=ISUM+IDAT
      CRAW=DAT(I)
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
