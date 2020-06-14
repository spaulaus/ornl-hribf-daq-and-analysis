C$PROG SWAPNIT   - Initializing routine for program SWAPO
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/25/02
C     ******************************************************************
C
      SUBROUTINE SWAPNIT
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      character*4 cnamprog(2)
      equivalence (cnamprog, namprog)
      DATA       cNAMPROG/'SWAP','O   '/
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
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'swapo.log',
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = IOS)
C
      CALL IOFERR(IOS)
C
c     CLOSE(UNIT=LOGUP,DISP='DELETE')
      CLOSE(UNIT=LOGUP)
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'swapo.log',
     &     STATUS     = 'REPLACE',
     &     IOSTAT     = IOS)
C
      CALL IOFERR(IOS)
C
      RETURN
      END
