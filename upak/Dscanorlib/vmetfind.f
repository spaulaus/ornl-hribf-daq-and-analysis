C$PROG VMETFIND  - Finds record for which VMET is GT VMETX & backs up 1
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 11/13/2004
C     ******************************************************************
C
      SUBROUTINE VMETFIND(VMETX)
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
      COMMON/SC06/ LIST(16384,2)
      INTEGER*2    LIST
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      COMMON/SC28/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC,CLONOF
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
      CHARACTER*4                                             CLONOF
C     ------------------------------------------------------------------
      INTEGER*4    IBUF(16384)
      EQUIVALENCE (IBUF,LIST)
C
      INTEGER*4    IERR,TIM,STAT,NBYRED,VMETX
C
      CHARACTER*4  KIND
C
      CHARACTER*16 ITM
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      IF(INTYP.NE.'LDF ') GO TO 1000   !tsfr LDF open
C
      IF(CLIDHI.LE.0)     GO TO 1030   !tsfr CLID defined
C
      INRECI=2                         !REWIND
C
  100 IF(MSGF.NE.'    ')  GO TO 1010   !tsfr CTRL/C
C
      CALL LDFREAD(LUINF,INRECI,IBUF,KIND,NBYRED,IERR)
C
      IF(IERR.EQ.999) THEN             !tsfr EOF
      INRECI=INRECI+1                  !if EOF, continue
      GO TO 100
      ENDIF
C
      CALL IOERR(IERR,STAT)            !Display any error msg
C
      IF(IERR.NE.0)       GO TO 1020   !If non-EOF error, abort
C
      IF(KIND.NE.'DATA') THEN          !tsfr DATA record
      INRECI=INRECI+1                  !if not, continue
      GO TO 100
      ENDIF
C
      CALL VMETGET(LIST,TIM)           !Try to get VME-time
C
      IF(TIM.LT.VMETX) THEN            !tsfr VMET-time GE VMETX
      INRECI=INRECI+1                  !if not, keep looking
      GO TO 100
      ENDIF
C
      CALL VMETFOR1(TIM,ITM)           !Insert commas to make readable
C
      WRITE(CMSSG,110)INRECI,ITM       !Display REC# and time
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(TIM.GT.VMETX) THEN            !tsfr BACKUP 1 needed
      INRECI=INRECI-1
      ENDIF
C
      WRITE(CMSSG,120)INRECI           !Display next record to be read
      CALL MESSLOG(LOGUT,LOGUP)
C
  110 FORMAT('Found at REC#',I10,'  first VME-time = ',A)
  120 FORMAT('Now   at REC#',I10)
C
      RETURN
C
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('LDF file not OPEN - CMD not processed')
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('Process interrupted via CTRL/C')
      GO TO 2000
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Error trying to find VME-time')
      GO TO 2000
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('VME-time parameter ID,s not defined - CMD not processed')
      GO TO 2000
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
