C$PROG LDFOPENA  - Opens Output list-data files for DATA ACQUISITION
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/15/2003
C     ******************************************************************
C
      SUBROUTINE LDFOPENA
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
      INTEGER*4    IWD,LWD,ITYP,NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
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
      COMMON/LM31/ SWAPLDF
      CHARACTER*4  SWAPLDF
C     ------------------------------------------------------------------
      CHARACTER*80 CWDRAW,CNAMF
C
      EQUIVALENCE (CWDRAW,IWDRAW)
C
      INTEGER*4    STRAPPEND,STRLEN,NXNB,LOST,IOS,IA,IDUM,IERR,NR,I
C
      INTEGER*4    KIND,NFW,STAT,JERR
C
      INTEGER*4    RECLVALU
C
      CHARACTER*4  KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      INTEGER*4  FSTAT,STATF,FILSIZ,FILSIZX,STATARA(13)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CNAMF=' '
C
      IA=NXNB(IWDRAW,4,80)
C
      IF(IA.LE.0) GO TO 1000
C
      LOST=STRAPPEND(CNAMF,CWDRAW(IA:STRLEN(CWDRAW)))
C
      IF(KMD.EQ.'OUF ') GO TO 200
C
      WRITE(CMSSG,100)
  100 FORMAT('Illegal command - ignored')
C
      GO TO 2000
C
C     ------------------------------------------------------------------
C     Open new list-data file for output
C     ------------------------------------------------------------------
C
  200 CALL LDFWRIT(LUOUF,'CLOS',0,IDUM,IERR)
      CLOSE(UNIT=LUOUF)
C
      OUFOP='NO  '
      OUTYP='    '
C
      OPEN(UNIT      = LUOUF,
     &     FILE      = CNAMF,
     &     STATUS    = 'NEW',
     &     ACCESS    = 'DIRECT',
     &     RECL      = RECLVALU(32776),
     &     IOSTAT    = IOS)
C
      IF(IOS.NE.0) THEN
      CALL IOFERR(IOS)
      WRITE(CMSSG,205)
  205 FORMAT('Unable to open NEW file - will open it as an OLD file')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 220
      ENDIF
C
      OUFOP='YES '
      OUTYP='LDF '
C
      CALL LDFWRIT(LUOUF,'INIT',0,IDUM,IERR)
C
      LDFONAM=CNAMF
C
      RETURN
C
C     ------------------------------------------------------------------
C     Open old list-data file for output
C     ------------------------------------------------------------------
C
  220 CALL LDFWRIT(LUOUF,'CLOS',0,IDUM,IERR)
      CLOSE(UNIT=LUOUF)
C
      OUFOP='NO  '
      OUTYP='    '
C
      OPEN(UNIT      = LUOUF,
     &     FILE      = CNAMF,
     &     STATUS    = 'OLD',
     &     ACCESS    = 'DIRECT',
     &     RECL      = RECLVALU(32776),
     &     IOSTAT    = IOS)
C
      IF(IOS.NE.0) THEN
      CALL IOFERR(IOS)
      GO TO 1000
      ENDIF
C
      READ(LUOUF,REC=1,IOSTAT=IOS)KIND,NFW
C
      IF(NFW.NE.8192) THEN
      CLOSE(UNIT=LUOUF)
      GO TO 1010
      ENDIF
C
      OUFOP='YES '
      OUTYP='LDF '
      LDFONAM=CNAMF
C
      CALL LDFWRIT(LUOUF,'OPEN',0,IDUM,IERR)
C
      CALL LDFCHEKA('OUT ',JERR)
C
      IF(JERR.NE.0) THEN
      CALL LDFWRIT(LUOUF,'CLOS',0,IDUM,IERR)
      CLOSE(UNIT=LUOUF)
      OUFOP='NO  '
      OUTYP='    '
      LDFONAM=' '
      GO TO 1020
      ENDIF
C
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('ERROR')
      GO TO 2000
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('Byte-order of existing LDF incompatible with this ',
     &       'platform - cmd rejected')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('Output file closed')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
C
 2000 CALL MESSLOG(LOGUT,LOGUP)
      RETURN
      END
