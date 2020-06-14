C$PROG TEXSAV    - Finds & stores text records (microprocessor code)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE TEXSAV(IERR)
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      COMMON/LM05/ IBUF(16384)
      INTEGER*4    IBUF
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      INTEGER*4    STRLEN,NXNB,LSNB
C
      INTEGER*4    ISTAT,IERR,IA,IB,NBRED,NFR,NBLR,LREC,NO,N,I,J
C
      INTEGER*4    LINE(20)
C
      CHARACTER*80 CIWD,CLINE
C
      INTEGER*4    ITEX(20,20)
C
      EQUIVALENCE (ITEX(1,1),IBUF(1)),(CIWD,IWDRAW),(CLINE,LINE)
C
      CHARACTER*4  FOUND,STAT
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     CREATE A FILE FOR STORAGE OF TEXT (MICROPROCESSOR PROG, ETC)
C     ------------------------------------------------------------------
C
      IERR=0
C
      IF(INTYP.EQ.'LDF ') THEN
      CALL TEXSAVLDF(IERR)
      RETURN
      ENDIF
C
      IF(LINO.NE.'YES ') GO TO 1920
C
      CLOSE(UNIT=LTX)
      IA=NXNB(IWDRAW,5,80)
      IF(IA.LE.0) GO TO 1900
      IB=LSNB(IWDRAW,IA,80)
      IF(IB.LE.0) GO TO 1900
C
      OPEN(UNIT       = LTX,
     &     FILE       = CIWD(IA:IB),
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL IOFERR(ISTAT)
                     GO TO 1910
                     ENDIF
C
C     ------------------------------------------------------------------
C     FIND THE NEXT 256 BYTE HEADER RECORD (PRIMARY HEADER)
C     ------------------------------------------------------------------
C
      FOUND='NO  '
C
 1530 CALL READUM(LUCI,IBUF,65536,NBRED,STAT)
C
      IF(STAT.NE.'GOOD') GO TO 2000
      IF(MSGF.NE.'    ') GO TO 2000
C
      IF(NBRED.EQ.1600)   GO TO 1535
      IF(FOUND.EQ.'NO  ') GO TO 1530
                          GO TO 1600
C
 1535 FOUND='YES '
C
      IF(ISWAB.EQ.'YES ') CALL SWAPPER(IBUF,NBRED)
      IF(ISWAH.EQ.'YES ') CALL SWAPHED(IBUF,NBRED)
C
      DO 1560 J=1,20
      CLINE=' '
      DO 1550 I=1,20
      LINE(I)=ITEX(I,J)
 1550 CONTINUE
      WRITE(LTX,1555)   CLINE(1:STRLEN(CLINE))
      WRITE(LOGUT,1556) CLINE(1:STRLEN(CLINE))
 1555 FORMAT(A)
 1556 FORMAT(1H ,A)
 1560 CONTINUE
      GO TO 1530
C
C
 1600 WRITE(CMSSG,1605)
 1605 FORMAT('DONE - HOPEFULLY!')
      CALL MESSLOG(LOGUT,LOGUP)
      CLOSE(UNIT=LTX)
      RETURN
C
C     ------------------------------------------------------------------
C     SEND ERROR MESSAGES AND RETURN 
C     ------------------------------------------------------------------
C
 1900 WRITE(CMSSG,1905)
 1905 FORMAT('SYNTAX ERROR IN FILENAME SPECIFICATION')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2000
C
 1910 WRITE(CMSSG,1915)
 1915 FORMAT('ERROR OPENING TEX-FILE')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2000
C
 1920 WRITE(CMSSG,1925)
 1925 FORMAT('INPUT TAPE NOT ASSIGNED - COMMAND IGNORED')
      CALL MESSLOG(LOGUT,LOGUP)
C
 2000 IERR=1
      CLOSE(UNIT=LTX)
      RETURN
      END
