C$PROG CHEKAS    - Checks line for valid ASCII (replaces invalid with ?)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE CHEKAS(LINE,N)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4   N,NN,IT,I
C
      INTEGER*2   LINE(1)
C
      INTEGER*4   X20,X3F,X7E
      DATA        X20,X3F,X7E/Z'20',Z'3F',Z'7E'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      NN=2*N
      DO 10 I=1,NN
      CALL ILBYTE(IT,LINE,I-1)
      IF(IT.GE.X20.AND.IT.LE.X7E) GO TO 10
      CALL ISBYTE(X3F,LINE,I-1)
   10 CONTINUE
      RETURN
      END
