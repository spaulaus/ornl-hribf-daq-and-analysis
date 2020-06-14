C$PROG SFUNK     - Calculates components for PKFIT and FITUM
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE SFUNK(IGO,NP,I1,I2,PAR,COMP)
C
      DIMENSION EXPO(1500),PAR(4),COMP(1)
C
      DATA NCALL/0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(NCALL.GT.0) GO TO 20
      NCALL=1
      DO 10 I=1,1500
      X=I-1
      X=X/50.0
      EXPO(I)=EXP(-X)
   10 CONTINUE
   20 CONTINUE
      WID=PAR(2)
      WSQ=WID*WID
      DO 150 I=I1,I2
      XG=I
      XX=(XG-PAR(1))/WID
      G=XX*XX
      AXX=ABS(XX)
      IF(XX)110,130,120
  110 G=G/(1.0+PAR(3)*AXX)
      GO TO 130
  120 G=G/(1.0+PAR(4)*AXX)
  130 NDX=50.0*G+1.5
      IF(NDX.GT.1500) COMP(I)=0.0
      IF(NDX.LE.1500) COMP(I)=EXPO(NDX)
  150 CONTINUE
      RETURN
      END
