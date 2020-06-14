C$PROG CHANFI
      SUBROUTINE CHANFI  (TABRD,TABCH,KMAX,RDIST,CHAN)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION TABRD(150), TABCH(150)
C      CCC
C   
C     ******************************************************************
C     THIS PROGRAM USES ALPHA PARTICLE CALIBRATION AND GIVES CHANNEL
C     NUMBER WHEN RDIST IS KNOWN
C     ******************************************************************
      DO 2 I=2,KMAX
C   
      IF (TABRD(I).GT.RDIST) GO TO 3
2     CONTINUE
3     IF (RDIST.LT.((TABRD(I)+TABRD(I-1))/2)) I=I-1
      I2=I
      IF (I2.LT.2) I2=2
      IF (I2.GT.(KMAX-1)) I2=KMAX-1
      I1=I2-1
      I3=I2+1
      CHAN=DFIT3(TABRD(I1),TABRD(I2),TABRD(I3),TABCH(I1),TABCH(I2),TAB
     1CH(I3),RDIST)
      RETURN
C   
      END
