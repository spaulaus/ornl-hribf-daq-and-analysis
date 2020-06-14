C$PROG KASEUP    - Converts specified portion of string to upper case
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE KASEUP(IBY,IA,IB)
C
      INTEGER*1 IBY(80)
C
      INTEGER*1 X20,X61,X7A
C
      DATA      X20,X61,X7A/Z'20',Z'61',Z'7A'/
C
      SAVE
C
      DO 20 I=IA,IB
      IT=IBY(I)
      IF(IT.EQ.X20) GO TO 20
      IF(IT.LT.X61.OR.IT.GT.X7A) GO TO 20
      IBY(I)=IBY(I)-X20
   20 CONTINUE
      RETURN
      END
