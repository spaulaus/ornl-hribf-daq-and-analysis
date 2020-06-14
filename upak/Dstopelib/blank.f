C$PROG BLANK     - Blanks 20 full words
C   
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE BLANK(IWD)
C
      INTEGER*4 IWD(*)
C
      DO 10 I=1,20
      IWD(I)='20202020'X
   10 CONTINUE
      RETURN
      END
