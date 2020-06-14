C$PROG SCANSTOP  - Stops scan process by setting MSGF flag
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 09/20/2004
C     ******************************************************************
C
      SUBROUTINE SCANSTOP
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
      COMMON/SC27/ BNDX
      INTEGER*4    BNDX
C     ------------------------------------------------------------------
C
      BNDX=1
C
      WRITE(CMSSG,10)
      CALL MESSLOG(LOGUT,LOGUP)
   10 FORMAT('SCANSTOP called from USER routine')
C
      MSGF='XXXX'
C
      RETURN
C
      END
