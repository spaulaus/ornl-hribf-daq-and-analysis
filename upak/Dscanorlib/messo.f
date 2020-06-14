C$PROG MESSO     - Non-batmama version of MESSO for compatibility
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/01/98
C     ******************************************************************
C
      SUBROUTINE MESSO(MESBUF,ICODE,ILOG)
C   
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C   
      INTEGER*4 MESBUF(13),ICODE,ILOG
C   
C     ==================================================================
C     NON-BATMAMA VERSION OF MESSO - FOR COMPATIBILITY
C   
C     ICODE - NOT USED
C     ILOG  = 1  SAYS DISPLAY ON VDT AND LINE PRINTER
C     ILOG  = 0  SAYS DISPLAY ON VDT ONLY
C     ==================================================================
C   
      WRITE(CMSSG,10)MESBUF
      IF(ILOG.EQ.0) CALL MESSLOG(LOGUT,0)
      IF(ILOG.NE.0) CALL MESSLOG(LOGUT,LOGUP)
   10 FORMAT(13A4)
      RETURN
      END
