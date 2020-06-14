C$PROG NDIGIT    - Returns the number of digits for I*4 number
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      INTEGER*4 FUNCTION NDIGIT(NUMB)
C
      IMPLICIT NONE
C
      INTEGER*4  NUM,NUMB,N,I
C
      SAVE
C
C     ------------------------------------------------------------------
C
      NUM=IABS(NUMB)
C
      N=1
      DO 10 I=1,10
      IF(NUM.EQ.0) GO TO 20
      N=N+1
      NUM=NUM/10
   10 CONTINUE
      N=10
C
   20 IF(NUMB.LT.0) N=N+1
      NDIGIT=N
      RETURN
      END
