C$PROG FITXNIT   - Initializing routine for FITX
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE FITXNIT
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
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/FT01/ IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      INTEGER*4    NERR,NCON,I
C     ------------------------------------------------------------------
      character*4  cnamprog(2)
      equivalence  (cnamprog, namprog)
      DATA         cNAMPROG/'FITX','    '/
C     ------------------------------------------------------------------
C
      SAVE
C
      CMSSG=' '
      MSGF='    '
      LISFLG='LON '
      LOGUT=6
      LOGUP=7
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'fitx.log',
     &     STATUS     = 'UNKNOWN')
C
c     CLOSE(UNIT=LOGUP,DISP='DELETE')
      CLOSE(UNIT=LOGUP)
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'fitx.log',
     &     STATUS     = 'REPLACE')
C
      CALL HELPNIT(IHEPF)
C
      CALL CTCNIT
C
      RETURN
C
      END
