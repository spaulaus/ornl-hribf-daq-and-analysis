C$PROG IOERR     - Reports I/O errors
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/13/99
C     ******************************************************************
C
      SUBROUTINE IOERR(ST,STAT)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    ST
C
      INTEGER*4    WTM_EOF,     IO_TIMEOUT,     ENOSPC,    EIO
C
      DATA         WTM_EOF/999/,IO_TIMEOUT/998/,ENOSPC/28/,EIO/5/
C
      CHARACTER*4  STAT
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      STAT='GOOD'                         !RESET ERROR CODE
      IF(ST.EQ.0) RETURN                  !TST FOR GOOD
C
      IF(ST.EQ.EIO)            GO TO 110  !TST FOR HARDWARE ERROR
      IF(ST.EQ.WTM_EOF)        GO TO 120  !TST FOR END OF FILE
      IF(ST.EQ.ENOSPC)         GO TO 130  !TST FOR END OF TAPE
      IF(ST.EQ.IO_TIMEOUT)     GO TO 140  !TST FOR IOTIMEOUT
                               GO TO 150  !LUMP ALL OTHERS TOGETHER
C
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES & RETURN
C     ------------------------------------------------------------------
C
  110 CALL IOSERR(ST)
      STAT='ERR '
      GO TO 200
  120 CALL IOSERR(ST)
      STAT='EOF '
      GO TO 200
  130 CALL IOSERR(ST)
      STAT='EOT '
      GO TO 200
  140 CALL IOSERR(ST)
      STAT='IOTO'
      GO TO 200
  150 CALL IOSERR(ST)
      STAT='ERR '
C
  200 RETURN
      END
