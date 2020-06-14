C$PROG WRITUM    - Writes one record to Output tape
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE WRITUM(C,IBUF,NBY,STAT)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(*),C,NBY,IERR,NBWT
C
      CHARACTER*4  STAT
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL MT_WRITE(C,IBUF,NBY,NBWT,IERR)
C
C
      CALL IOERR(IERR,STAT)
      RETURN
      END
