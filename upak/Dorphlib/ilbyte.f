C$PROG ILBYTE    - Loads one byte from specified location
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/15/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE ILBYTE(IT,IBY,NB)
C
      BYTE IBY(*)
C
      SAVE
C
      IT=IBY(NB+1)
      RETURN
      END
