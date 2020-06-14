C$PROG CHEKAS    - Checks line for valid ASCII (replaces invalid with ?)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/13/99
C     ******************************************************************
C
      SUBROUTINE CHEKAS(LINE,N)
C
      IMPLICIT NONE
C
C     ==================================================================
      INTEGER*2 LINE(1)
C
      INTEGER*4 N,NN,IT,I
C
      INTEGER*4 X20,X7E,X3F
C
      DATA      X20,X7E,X3F/'20'X,'7E'X,'3F'X/
C
      SAVE
C
C     ==================================================================
C
      NN=2*N
      DO 10 I=1,NN
      IT=0
      CALL ILBYTE(IT,LINE,I-1)
      IF(IT.GE.X20.AND.IT.LE.X7E) GO TO 10
      CALL ISBYTE(X3F,LINE,I-1)
   10 CONTINUE
      RETURN
      END
