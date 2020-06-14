C$PROG SHELCO    - Shell correction factor - from ???
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION SHELCO(E,Z)
C
      COMMON/ST03/ AA(12,92)
C
      DIMENSION A(12)
C
      IZ=Z+0.5
      DO 10 I=1,12
      A(I)=AA(I,IZ)
   10 CONTINUE
C
      SUM=0.0
      ELOG=ALOG(E)
      DO 30 I=1,5
      J=I-1
      K=J+8
      SUM=SUM+A(K)*ELOG**J
   30 CONTINUE
      SHELCO=SUM
      RETURN
      END
