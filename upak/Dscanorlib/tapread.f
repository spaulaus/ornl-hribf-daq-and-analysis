C$PROG TAPREAD   - Reads one record from input tape - for Mips SCANU
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/13/99
C     ******************************************************************
C
      SUBROUTINE TAPREAD(C,IBUF,NBY,NBRED,STAT)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4 IBUF(*)
C
      INTEGER*4    C,NBY,NBRED,STAT,IERR
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL MT_READ(C,IBUF,NBY,NBRED,IERR)
C
      CALL IOERR(IERR,STAT)
      RETURN
      END
