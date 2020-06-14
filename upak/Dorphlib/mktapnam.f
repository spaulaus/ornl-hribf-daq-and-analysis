C$PROG MKTAPNAM  - Makes a "tape name" by annexing /dev/n
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE MKTAPNAM(CNAMI,CNAMO)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    STRAPPEND,LOST
C
      CHARACTER*8  CNAMI
C
      CHARACTER*80 CNAMO
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CNAMO='/dev/n'
C
      CALL STRLOWER(CNAMI)
C
      LOST=STRAPPEND(CNAMO,CNAMI)
C
      RETURN
      END
