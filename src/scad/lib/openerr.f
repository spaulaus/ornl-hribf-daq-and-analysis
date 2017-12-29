C$PROG OPENERR   - Displays OPEN-ERROR message for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE OPENERR(IOS)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(IOS.EQ.0) RETURN
C
      WRITE(CMSSG,10)IOS
   10 FORMAT('ERROR OPENING FILE - ZSTAT =',Z10)
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
