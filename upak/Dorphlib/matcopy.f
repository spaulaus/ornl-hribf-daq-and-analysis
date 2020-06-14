C$PROG MATCOPY   - Matrix copy routine
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/01/2003
C     ******************************************************************
C
      SUBROUTINE MATCOPY(NROW,NCOL,A,B)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      REAL*8       A(50,50),B(50,50)
C
      INTEGER*4    I,J,NROW,NCOL
C
C     ------------------------------------------------------------------
C     Copies matrix A into Matrix B
C
C     Matrix A is of order NROW by NCOL
C
C     Matrix B is of order NROW by NCOL
C     ------------------------------------------------------------------
C
      DO 30 J=1,NCOL
C
      DO 20 I=1,NROW
C
      B(I,J)=A(I,J)
C
   20 CONTINUE
C
   30 CONTINUE
C
      RETURN
C
      END
