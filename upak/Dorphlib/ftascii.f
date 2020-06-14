C$PROG FTASCII   - Displays REAL*4 numbers in ASCII (various fmts)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE FTASCII(FV,ANUM,NPL)
C
      REAL*8 DV
C
      INTEGER*4 ANUM(3)
C
      SAVE
C
      DV=FV
      CALL DFASCII(DV,ANUM,NPL)
      RETURN
      END
