C$PROG SQUEZR    - Squeezes characters "right" in specified field
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/25/2002
C     ******************************************************************
C
      SUBROUTINE SQUEZR(IWD,IA,IB)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    IA,IB,I,J,K
C
      BYTE         IWD(*),JWD(80)
C
      BYTE         X20
      DATA         X20/'20'X/
C
C     ------------------------------------------------------------------
C
      DO 10 I=IA,IB
      JWD(I)=IWD(I)
      IWD(I)='20'X
   10 CONTINUE
C
      J=IB+1
      K=IB+1
      DO 20 I=IA,IB
      J=J-1
      IF(JWD(J).EQ.X20) GO TO 20
      K=K-1
      IWD(K)=JWD(J)
   20 CONTINUE
      RETURN
      END
