C$PROG SWAPHED   - Swaps selected bytes in header record
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE SWAPHED(IBUF,NBYT)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(*),NBYT
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(ISWAH.NE.'YES ') RETURN
C
      IF(NBYT.EQ. 256)    THEN
                          CALL SWAPF(IBUF,33,64)
                          RETURN
                          ENDIF
C
      RETURN
      END
