C$PROG NEARBAN   - Returns ID of banana nearest to x,y location
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      SUBROUTINE NEARBAN(IDW,MODE,X,Y,ID,NDX)
C   
C     ------------------------------------------------------------------
      COMMON/PL09/ XL(64,21),YL(64,21),NXYL(21),IBL(21),
     &             IBWN(21),LNBAN(4,21),MAXXYL
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      ID=0
      NDX=0
      RMIN=1.0E8
C   
      MAX=MAXXYL
      IF(MODE.EQ.2) MAX=MAXXYL+1
C   
      DO 100 N=1,MAX
C   
      IF(IBL(N).LT.0)    GO TO 100
      IF(IBWN(N).NE.IDW) GO TO 100
C
      NDO=NXYL(N)
C   
      DO 50 I=1,NDO
      IF(IMOUT(IDW,XL(I,N),YL(I,N)).NE.0) GO TO 50
      DX=X-XL(I,N)
      DY=Y-YL(I,N)
      R=SQRT(DX*DX+DY*DY)
      IF(R.LT.RMIN) THEN
                    ID=IBL(N)
                    NDX=N
                    RMIN=R
                    ENDIF
   50 CONTINUE
C   
  100 CONTINUE
      RETURN
      END
