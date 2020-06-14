C$PROG IPCOPEN   - Opens/closes SHM for use by routine INPUTIPC
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 07/16/99
C     ******************************************************************
C
      SUBROUTINE IPCOPEN(KMD,IERR)
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
      COMMON/SC05/ NHWH,LSTL,LNBY,MAXIP,NSKIP,ISWAB,LFORM                      
      INTEGER*4    NHWH,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,LFORM
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      COMMON/SC22/ IPCNAM(20),LDFNAM(20),TAPNAM(20),BANNAM(20)
      INTEGER*4    IPCNAM,    LDFNAM,    TAPNAM,    BANNAM
C     ------------------------------------------------------------------
      INTEGER*4    IERR,STRLEN
C
      CHARACTER*4  KMD,KOM
C
      CHARACTER*80 CIPCNAM
C
      EQUIVALENCE (CIPCNAM,IPCNAM)
C
      EQUIVALENCE (KOM,LWD(1,1))
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(KMD.EQ.'SHM ') GO TO 100
      IF(KMD.EQ.'IPC ') GO TO 100
      IF(KMD.EQ.'ACQ ') GO TO 100
      IF(KMD.EQ.'CLOI') GO TO 200
C
      RETURN
C
C     ------------------------------------------------------------------
C     SHM open and close operations
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
C     ------------------------------------------------------------------
C     Close anything that is open - TAPE, SHM, LDF
C     ------------------------------------------------------------------
C
      IF(INTYP.EQ.'TAPE') THEN          !Close input tape if open
      KOM='CLOT'
      CALL TAPOPEN(IERR)
C
      IF(IERR.NE.0) THEN
      WRITE(CMSSG,105)
  105 FORMAT('I can not seem to close the open TAPE')
      CALL MESSLOG(LOGUT,0)
      ENDIF
      ENDIF
C
      IF(INTYP.EQ.'SHM ') THEN          !Close input SHM  if open
      CALL CLOSEIPC()
      ENDIF
C
      IF(INTYP.EQ.'LDF ') THEN          !Close input LDF  if open
      CLOSE(UNIT=LUINF)
      ENDIF
C
      INTYP='    '
C
C     ------------------------------------------------------------------
C     If no SHM name specified, try to open the default acquisition 
C     stream.
C     ------------------------------------------------------------------
C
      IF(NF.LT.2) THEN
      CALL GETENV("VME",CIPCNAM)
      IF(CIPCNAM(1:1).EQ.' ') GO TO 500
      GO TO 110
      ENDIF
C
      CIPCNAM=' '
      IPCNAM(1)=LWD(1,2)
      IPCNAM(2)=LWD(2,2)
      CALL STRLOWER(CIPCNAM)
C
  110 CALL OPENIPC('READ',IPCNAM,LNBY,IERR)
      LSTL=LNBY/2
C
      IF(IERR.NE.0) GO TO 510
C
      WRITE(CMSSG,115)CIPCNAM(1:STRLEN(CIPCNAM))
C
  115 FORMAT('SHM connection opened to ',A)
C
      CALL MESSLOG(LOGUT,0)
C
      INTYP='SHM '
C
      RETURN
C
C     ------------------------------------------------------------
C     CLOI  command - close the IPC
C     ------------------------------------------------------------
C
  200 IF(INTYP.EQ.'SHM ') THEN
      CALL CLOSEIPC()
      INTYP='    '
      ENDIF
      RETURN
C
C     ------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('You MUST specify the name of the VME processor that'
     &      ,' you are using!')
      GO TO 1000
C
  510 WRITE(CMSSG,515)CIPCNAM(1:STRLEN(CIPCNAM))
  515 FORMAT('Cannot open SHM connection to ',A)
      GO TO 1000
C
 1000 CALL MESSLOG(LOGUT,0)
      IERR=1
      RETURN
      END
