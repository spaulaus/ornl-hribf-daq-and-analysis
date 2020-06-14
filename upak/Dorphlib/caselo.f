C$PROG CASELO    - Converts ASCII string (IA to IB) to lower case
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/13/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE CASELO(IBY,IA,IB)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      INTEGER*1 IBY(*)                      !Was type BYTE on Alpha
C
      INTEGER*1 X41,X5A,X20
C
      DATA      X41,X5A,X20/Z'41',Z'5A',Z'20'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 20 I=IA,IB
      IT=IBY(I)
      IF(IT.LT.X41.OR.IT.GT.X5A) GO TO 20
      IBY(I)=IT+X20
   20 CONTINUE
      RETURN
      END
