C$PROG CLOSEALL  - Closes any input tape, file or ipc that is open
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/05/99
C     ******************************************************************
C
      SUBROUTINE CLOSEALL
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      INTEGER*4    IERR
C
      CHARACTER*4  KMD,KMDSAV
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(INTYP.EQ.'TAPE') THEN
      KMDSAV=KMD
      KMD='CLOT'
      CALL TAPOPEN(IERR)
      INTYP='    '
      KMD=KMDSAV
      RETURN
      ENDIF
C
C
      IF(INTYP.EQ.'LDF ') THEN
      CLOSE(UNIT=LUINF)
      INTYP='    '
      RETURN
      ENDIF
C
      IF(INTYP.EQ.'UDF ') THEN
      CLOSE(UNIT=LUINF)
      INTYP='    '
      RETURN
      ENDIF
C
      IF(INTYP.EQ.'SHM ') THEN
      CALL IPCOPEN('CLOI',IERR)
      INTYP='    '
      RETURN
      ENDIF
C
      END
