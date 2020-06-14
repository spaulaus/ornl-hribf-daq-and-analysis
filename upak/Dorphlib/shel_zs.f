C$PROG SHEL_ZS   - Computes shell correction to S(E) - from Ziegler
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION SHEL_ZS(E,Z)
C
      COMMON/QSQ001/ AA(12,92)
C
      DIMENSION A(12)
C
      SAVE
C
C     ------------------------------------------------------------------
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
      SHEL_ZS=SUM
      RETURN
      END
