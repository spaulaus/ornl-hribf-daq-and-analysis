C$PROG LDFOPEN   - Opens Input & Output list-data files for LEMOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE LDFOPEN
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
      DATA         LDFONAM,LDFINAM/' ',' '/
C     ------------------------------------------------------------------
      CHARACTER*80 CWDRAW,CNAMF
C
      EQUIVALENCE (CWDRAW,IWDRAW)
C
      INTEGER*4    STRAPPEND,STRLEN,NXNB,LOST,IOS,IA,IDUM,IERR,NR,I
C
      INTEGER*4    KIND,NFW,STAT
C
      INTEGER*4    RECLVALU
C
      CHARACTER*4  KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
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

*
*    Remove leading spaces from the filename
*
      do i=1,80
        if (cnamf(i:i) .ne. ' ') go to 176
      enddo
176   if (i .ne. 1) cnamf = cnamf(i:)
*
*   Require that user specify a full path file name
*
      if (cnamf(1:1) .ne. '/') go to 4030

C
      IF(KMD.EQ.'INF ') GO TO 100
      IF(KMD.EQ.'OUF ') GO TO 200
C
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Open list-data file for input
C     ------------------------------------------------------------------
C
  100 CLOSE(UNIT=LUINF)
      INFOP='NO  '
      INTYP='    '
C
      OPEN(UNIT      = LUINF,
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
      INFOP='YES '
      INTYP='LDF '
      LDFINAM=CNAMF
C
      CALL LDFREAD(LUINF,1,INDIR,KIND,NFW,STAT)
C
      INRECI=1
C
      RETURN
C
C     ------------------------------------------------------------------
C     Open new list-data file for output
C     ------------------------------------------------------------------
C
  200 CALL LDFWRIT(LUOUF,'CLOS',0,IDUM,IERR)
      CLOSE(UNIT=LUOUF)
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
      OUFOP='YES '
      OUTYP='LDF '
C
      CALL LDFWRIT(LUOUF,'OPEN',0,IDUM,IERR)
C
      LDFONAM=CNAMF
C
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('ERROR')
      CALL MESSLOG(LOGUT,LOGUP)
      RETURN
*
 4030 write(cmssg,4031) cnamf(1:strlen(cnamf))
 4031 format('file name : ',a)
      CALL MESSLOG(LOGUT,LOGUP)
      write(cmssg,4035)
 4035 format('You MUST specify FULL PATH for the file')
      CALL MESSLOG(LOGUT,LOGUP)
      return
      END
