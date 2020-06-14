C$PROG IOSMES    - Calls GET_LUX_ERROR to get IOS error messages
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/16/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE IOSMES(IOS,MSER)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      CHARACTER*80 MESR
C
      SAVE
C
C     ------------------------------------------------------------------
C
      MESR=' '
C
      IF(IOS.EQ.0) RETURN
C
      CALL GET_fortran_ERROR(IOS,MSER)
C
      RETURN
C
CX    IF(IOS.GE.1.AND.IOS.LT.100) THEN
CX        CALL GET_SYS_ERROR(IOS,MSER)
CX    ELSE
CX        CALL GET_F77_ERROR(IOS,MSER)
CX    ENDIF
C
      RETURN
      END
