C$PROG COPYFTF   - Copies one ldf-file to another
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE COPYFTF(KMD,NFE,IERR)
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
      COMMON/LM01/ LIN,KFI,LINO,LOU(3),KFO(3),LOUO(3),LTX,LML,LMLO,
     &             KMDS,LUCI,LUCO(3)
C
      INTEGER*4    LIN,         LOU,                  LTX,LML,
     &                  LUCI,LUCO
C
      CHARACTER*4      KFI,LINO,       KFO,   LOUO,           LMLO
      CHARACTER*4  KMDS
C     ------------------------------------------------------------------
      COMMON/LM04/ IONP,JONP(3)
      CHARACTER*4  IONP,JONP
C     ------------------------------------------------------------------
      COMMON/LM07/ NPAR,LSTL,LNBY,MAXIP,NSKIP,ISWAB,ISWAH,LFORM
      INTEGER*4    NPAR,LSTL,LNBY,MAXIP,NSKIP
      CHARACTER*4                             ISWAB,ISWAH,LFORM
C     ------------------------------------------------------------------
      COMMON/LM14/ OUPTR(3),OOFSET(3),OUSIZ(3),OUBUF(16384,2,3)
      INTEGER*4    OUPTR,   OOFSET,   OUSIZ
      INTEGER*2                                OUBUF
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      INTEGER*4    JBUF(16384)
      EQUIVALENCE (JBUF,OUBUF)
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD,LRIT,STAT
C
      INTEGER*4    NFE,IERR
C
      INTEGER*4    MSGX,NBYT,NFW,KIND,IV,I
C
      INTEGER*4    NEOF,NRTF,NRCOP,NFCOP,NREC,NFILE
C
      DATA         NEOF,NRTF,NRCOP,NFCOP,NREC,NFILE/6*0/
C     ------------------------------------------------------------------
      INTEGER*4    LUI,LUO,MXRECI
C
      EQUIVALENCE (LUI,LUCI),(LUO,LUCO(1)),(MXRECI,INDIR(2))
C
      SAVE
C
C
C     ------------------------------------------------------------------
C     COPY N (FILES)  OR  CREC N (RECORDS) FROM one LDF-file to another
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     Determine how many FILES or RECORDS to copy
C     ------------------------------------------------------------------
C
      IERR=0
C
      CALL LDFOOK(IERR)          !Tst for ldf-output file write-ready 
      IF(IERR.NE.0) RETURN       !If not, RETURN
C
      MSGX=0
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
C
      IF(INFOP.NE.'YES ')    GO TO 1020
C
      IF(OUFOP.NE.'YES ')    GO TO 1030
C
      IF(KMD.EQ.'CC  ')      GO TO 100
C
      NFILE=1
      NREC=100000000
      IF(KMD.EQ.'CREC') NREC=1
      IF(NFE.EQ.1)       GO TO 50
      IF(NFE.NE.2)       GO TO 1010
      CALL IVALU(LWD(1,2),IV,IERR)
      IF(IERR.NE.0)      GO TO 1010
      IF(IV.LT.1)        GO TO 1010
      IF(KMD.EQ.'COPY') NFILE=IV
      IF(KMD.EQ.'CREC') NREC =IV
C
   50 NFCOP=0
      NRCOP=0
      NRTF=0
C
C     ------------------------------------------------------------------
C     I/O loop starts here
C     ------------------------------------------------------------------
C
  100 IF(MSGF.NE.'    ') THEN
                         MSGF='    '
                         MSGX=1
                         GO TO 550
                         ENDIF
C
      INRECI=INRECI+1
C
      IF(INRECI.GT.MXRECI) RETURN
C
      CALL LDFREAD(LUINF,INRECI,JBUF,KIND,NFW,STAT)
C
      NBYT=4*NFW
C
      IF(STAT.NE.'GOOD') GO TO 600
C
C
      IF(ISWAB.EQ.'YES ') CALL SWAPPER(JBUF,NBYT)
      IF(ISWAH.EQ.'YES ') CALL SWAPHED(JBUF,NBYT)
C
      NEOF=0
      IF(NBYT.NE.256)    GO TO 200
      WRITE(CMSSG,105)(JBUF(I),I=9,23),JBUF(33)
      CALL MESSLOG(LOGUT,LOGUP)
  105 FORMAT(15A4,' - #',I12)
C
C
  200 CALL LDFWRIT(LUOUF,KIND,NBYT,JBUF,IERR)
C
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
C
      IF(IERR.NE.0) GO TO 550
C
      NRCOP=NRCOP+1
      NRTF=NRTF+1
      IF(NRCOP.LT.NREC)  GO TO 100
C
  550 WRITE(CMSSG,555)NRTF,NRCOP
      CALL MESSLOG(LOGUT,LOGUP)
  555 FORMAT('RECS-THIS-FILE, TOTAL-RECS COPIED =',2I8)
  560 FORMAT('NUMBER OF            FILES COPIED =',I8)
C
      CALL LDFILMAR(2,2,IERR)
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
      IF(IERR.NE.0)      GO TO 1000
      IF(STAT.NE.'GOOD') GO TO 1000
      IF(MSGX.NE.0)      GO TO 1000
      RETURN
C
  600 WRITE(CMSSG,555)NRTF,NRCOP
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(STAT.EQ.'EOF ')  GO TO 610
      IF(STAT.EQ.'EOT ')  GO TO 610
      IF(STAT.EQ.'EOV ')  GO TO 610
C
      CALL LDFILMAR(2,2,IERR)
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
      GO TO 1000
C
  610 NEOF=NEOF+1
C
      IF(LRIT.NE.'EOF ') THEN
      CALL LDFILMAR(2,1,IERR)
      ENDIF
C
      LRIT='EOF '                          !SET LAST-RIT-FLG='EOF '
      NFCOP=NFCOP+1
      NRTF=0
C
      WRITE(CMSSG,560)NFCOP
      IF(NEOF.NE.2) CALL MESSLOG(LOGUT,LOGUP)
C
      WRITE(CMSSG,640)
      IF(NEOF.GE.2) CALL MESSLOG(LOGUT,LOGUP)
C
      IF(IERR.NE.0)      GO TO 1000
      IF(NFCOP.GE.NFILE) RETURN
      IF(STAT.EQ.'EOF ') GO TO 620
      IF(STAT.EQ.'EOT ') RETURN
      IF(STAT.EQ.'EOV ') RETURN
      GO TO 1000
C
  620 IF(NEOF.LT.2)      GO TO 100
      RETURN
C
  640 FORMAT('DOUBLE END-OF-FILE ENCOUNTERED')
C
C     ------------------------------------------------------------------
C     Error returns
C     ------------------------------------------------------------------
C
 1000 IERR=1
      RETURN
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - IGNORED')
      GO TO 1200
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('INPUT LDF-FILE NOT ASSIGNED - COMMAND IGNORED')
      GO TO 1200
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('OUTPUT LDF-FILE NOT ASSIGNED - COMMAND IGNORED')
C
 1200 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
