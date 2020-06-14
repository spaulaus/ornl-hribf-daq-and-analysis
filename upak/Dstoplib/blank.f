C$PROG BLANK     - Blanks 20 full words
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      SUBROUTINE BLANK(IWD)
C
      INTEGER*4 IWD(1)
C
      DO 10 I=1,20
      IWD(I)='20202020'X
   10 CONTINUE
      RETURN
      END
