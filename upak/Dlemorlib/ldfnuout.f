C$PROG LDFNUOUT  - Deletes & re-opens ldf-output file with same name
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE LDFNUOUT
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
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      COMMON/LM26/ LDFONAM,LDFINAM
      CHARACTER*80 LDFONAM,LDFINAM
C     ------------------------------------------------------------------
      INTEGER*4    IOS,IDUM,IERR,ISTAT
C
      INTEGER*4    RECLVALU,STRLEN
C
      CHARACTER*4  RESPONSE
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(OUFOP.NE.'YES ')  GO TO 500
      IF(LDFONAM.EQ.' ')   GO TO 500
C
      CALL DINGER(3)
C
      WRITE(CMSSG,50)
   50 FORMAT(9('--------'))
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,100)(LDFONAM(1:STRLEN(LDFONAM)))
  100 FORMAT('Are you SURE you want to DELETE & REALLOCATE - ',A)
      CALL MESSLOG(LOGUT,LOGUP)
C
      CALL DINGER(3)
C
      WRITE(CMSSG,110)
  110 FORMAT('To CONFIRM Type: yes')
      CALL MESSLOG(LOGUT,LOGUP)
C
      CALL DINGER(3)
C
      WRITE(CMSSG,120)
  120 FORMAT('To ABORT   Type: anything else')
      CALL MESSLOG(LOGUT,LOGUP)
C
      CALL MESSLOG(LOGUT,LOGUP)
C
      CALL WAIT(200,1,ISTAT)
C
      WRITE(6,130)
  130 FORMAT(1H ,'Do you confirm?->',$)
      READ(5,140)RESPONSE
  140 FORMAT(A)
C
      IF(RESPONSE.EQ.'YES ') GO TO 200
      IF(RESPONSE.EQ.'yes ') GO TO 200
C
      GO TO 530
C
C 200 CLOSE(UNIT=LUOUF,DISPOSE='DELETE')
  200 CLOSE(UNIT=LUOUF)
C
      OUFOP='NO  '
      OUTYP='    '
C
      OPEN(UNIT      = LUOUF,
     &     FILE      = LDFONAM,
     &     STATUS    = 'REPLACE',
     &     ACCESS    = 'DIRECT',
     &     RECL      = RECLVALU(32776),
     &     IOSTAT    = IOS)
C
      IF(IOS.NE.0) THEN
      CALL IOFERR(IOS)
      GO TO 510
      ENDIF
C
      OUFOP='YES '
      OUTYP='LDF '
C
      CALL LDFWRIT(LUOUF,'INIT',0,IDUM,IERR)
C
      IF(IERR.NE.0) GO TO 540
C
      GO TO 550
C
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
  500 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,505)
  505 FORMAT('Output LDF-filename undefined - command ignored')
      GO TO 1000
C
  510 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,515)(LDFONAM(1:STRLEN(LDFONAM)))
  515 FORMAT('Error trying or open - ',A)
      GO TO 1000
C
  530 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,535)
  535 FORMAT('ELDF request not confirmed - command ignored')
      GO TO 1000
C
  540 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,545)(LDFONAM(1:STRLEN(LDFONAM)))
  545 FORMAT('Error trying to initialize - ',A)
      GO TO 1000
C
  550 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,555)(LDFONAM(1:STRLEN(LDFONAM)))
  555 FORMAT('File: ',A,' - has been reallocated')
      GO TO 1000
C
C
 1000 CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,50)
      CALL MESSLOG(LOGUT,LOGUP)
      CALL DINGER(3)
      RETURN
      END
