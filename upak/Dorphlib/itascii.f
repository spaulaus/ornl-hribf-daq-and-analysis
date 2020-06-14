C$PROG ITASCII   - Integer to ASCII converter
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE ITASCII(IV,ANUM,NPL)
C
      REAL*8 DV
C
      INTEGER*4 ANUM(3)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      DV=IV
C
      CALL DFASCII(DV,ANUM,NPL)
C
      RETURN
C
      END
