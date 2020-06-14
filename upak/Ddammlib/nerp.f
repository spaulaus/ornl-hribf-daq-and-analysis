C$PROG NERP      - Returns index of peak on screen whose loc is nearest
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      FUNCTION NERP(X)
C   
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM07/ WINL(4),KSCAL,LASIDW
C   
      DIMENSION XPOS(500)
C   
      EQUIVALENCE (XPOS(1),PAT(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION RETURNS INDEX OF PEAK ON SCREEN WHOSE LOCATION IS NEAREST
C     NERP = 0, SAYS NO PEAKS ON SCREEN
C     ------------------------------------------------------------------
C   
      IP=0
      IF(NPAT.LT.1) GO TO 100
      DIF=20000.0
      DO 20 I=1,NPAT
      IF(XPOS(I).LT.WINL(1).OR.XPOS(I).GT.WINL(2)) GO TO 20
      DIFT=ABS(X-XPOS(I))
      IF(DIF.LT.DIFT) GO TO 20
      IP=I
      DIF=DIFT
   20 CONTINUE
C   
  100 NERP=IP
      RETURN
      END
