C$PROG LDFOPEN   - Opens Input List-Data-Files for SCANOR
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/12/2003
C     ******************************************************************
C
      SUBROUTINE LDFOPEN(IERR)
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
      COMMON/SC15/ NCEOF,LAUTO
      INTEGER*4    NCEOF
      CHARACTER*4        LAUTO
C     ------------------------------------------------------------------
      COMMON/SC16/ INDIR(8192),INTYP,INRECI,LUINF
      INTEGER*4    INDIR,            INRECI,LUINF
      CHARACTER*4              INTYP
C     ------------------------------------------------------------------
      COMMON/SC22/ IPCNAM(20),LDFNAM(20),TAPNAM(20),BANNAM(20)
      INTEGER*4    IPCNAM,    LDFNAM,    TAPNAM,    BANNAM
C     ------------------------------------------------------------------
      COMMON/SC26/ SWAPLDF
      CHARACTER*4  SWAPLDF
C     ------------------------------------------------------------------
      CHARACTER*80 CWDRAW,CNAMF
C
      EQUIVALENCE (CWDRAW,IWDRAW),(CNAMF,LDFNAM)
C
      INTEGER*4    STRAPPEND,STRLEN,NXNB,LOST,IOS,IA,IDUM,IERR,NR,I
C
      INTEGER*4    KIND,NBYT,STAT,JERR
C
      INTEGER*4    RECLVALU
C
      CHARACTER*4  KMD
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      INTEGER*4  FSTAT,STATF,FILSIZ,STATARA(13)
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IERR=0
C
      CNAMF=' '
C
      IA=NXNB(IWDRAW,5,80)
C
      IF(IA.LE.0) GO TO 1000
C
      LOST=STRAPPEND(CNAMF,CWDRAW(IA:STRLEN(CWDRAW)))
C
      IF(KMD.EQ.'FILE') GO TO 100
      IF(KMD.EQ.'LDF ') GO TO 100
C
      GO TO 1000
C
C     ------------------------------------------------------------------
C     Open List-Data-File for input
C     ------------------------------------------------------------------
C
  100 CLOSE(UNIT=LUINF)
      INTYP='    '
C
      NCEOF=0
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
      INTYP='LDF '
C
      SWAPLDF='NO  '
C
      CALL LDFREAD(LUINF,1,INDIR,KIND,NBYT,STAT)
C
      INRECI=2
C
      IF(NBYT.EQ.32768) THEN
      CALL LDFCHEK(JERR)
      RETURN
      ENDIF
C
      WRITE(CMSSG,105)
  105 FORMAT(9('--------'))
      CALL MESSLOG(LOGUT,LOGUP)
      CALL DINGER(3)
      WRITE(CMSSG,120)
  120 FORMAT('LDF byte-order is INCOMPATIBLE with this platform')
      CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(LOGUT,125)
  125 FORMAT(1H )
C
      WRITE(CMSSG,130)
  130 FORMAT('Records will be AUTOMATICALLY byte-swapped by input ',
     &       'routine')
      CALL MESSLOG(LOGUT,LOGUP)
C
      CALL DINGER(3)
C
      DO 140 I=1,8192
      CALL LDFSWAP(2,INDIR(I))
  140 CONTINUE
      SWAPLDF='YES '
      CALL LDFCHEK(JERR)
C
      IF(JERR.EQ.0) THEN
      WRITE(CMSSG,105)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
      RETURN
C
C     ------------------------------------------------------------------
C     Send error messages
C     ------------------------------------------------------------------
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('LDFOPEN ERROR')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
