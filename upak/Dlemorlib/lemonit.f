C$PROG LEMONIT   - Initializing routine for LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE LEMONIT
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
      COMMON/LM06/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      character*4  cnamprog(2)
      equivalence  (cnamprog, namprog)
      DATA         cNAMPROG/'LEMO','R   '/
C
      INTEGER*4    NERR,NCON,I
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CMSSG=' '
      MSGF='    '
      LISFLG='LON '
      LOGUT=6
      LOGUP=7
C
      OPEN(UNIT       = 7,
     &     FILE       = 'lemor.log',
     &     STATUS     = 'UNKNOWN',
     &     ACCESS     = 'APPEND')
C
      CALL HELPNIT(IHEPF)
C
      CALL CTCNIT
C
      RETURN
C
      END
