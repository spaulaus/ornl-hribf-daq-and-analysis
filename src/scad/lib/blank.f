C$PROG BLANK     - Sets specified region of array to BLANK
C
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE BLANK(IBUF,IA,IB)
C
      INTEGER*4 IBUF(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DO 10 I=IA,IB
      IBUF(I)='20202020'X
   10 CONTINUE
      RETURN
      END
