C$PROG MATMULT   - Matrix multiply routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/01/2003
C     ******************************************************************
C
      SUBROUTINE MATMULT(N,M,L,A,B,C)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      REAL*8       A(50,50),B(50,50),C(50,50)
C
      REAL*8       SUM
C
      INTEGER*4    I,J,K,L,M,N
C
C     ------------------------------------------------------------------
C     Matrix A is of order N x L
C
C     Matrix B is of order L x M
C
C     Matrix C is of order N x M
C
C     C = A * B
C
C     For square matrices, L = M = N
C     ------------------------------------------------------------------
C
      DO 30 J=1,M
C
      DO 20 I=1,N
C
      SUM=0.0
C
      DO 10 K=1,L
C
      SUM=SUM+A(I,K)*B(K,J)
C
   10 CONTINUE
C
      C(I,J)=SUM
C
   20 CONTINUE
C
   30 CONTINUE
C
      RETURN
C
      END
