C$PROG COPYTOF   - Copys list-data from tape to disk file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE COPYTOF(KMD,NFE,IERR)
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
      COMMON/LM09/ KINPUT,INRECN
      CHARACTER*4  KINPUT
      INTEGER*4           INRECN
C     ------------------------------------------------------------------
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM24/ LAUTO,INRECL
      CHARACTER*4  LAUTO
      INTEGER*4          INRECL
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD,LRIT,STAT,MODE,RECTST
C
      INTEGER*4    NFE,IERR
C
      INTEGER*4    MSGX,IV,NBYT,JERR,I
C
      INTEGER*4    NEOF,NRTF,NRCOP,NFCOP,NREC,NFILE
C
      DATA         NEOF,NRTF,NRCOP,NFCOP,NREC,NFILE/6*0/
C     ------------------------------------------------------------------
      INTEGER*4    JBUF(8192)
C
      INTEGER*4    IDUM,TRECL,TRECLFW,JDX,JDXMAX,NBYTF,NFWF
C
      INTEGER*4    BLANK,SCAL
      character*4  cblank, cscal
      equivalence  (cblank, blank), (cscal, scal)
      DATA         cBLANK,cSCAL/'    ','SCAL'/
C
      SAVE
C
C     ------------------------------------------------------------------
C     COPY N (FILES)  OR  CREC N (RECORDS) FROM INPUT TO OUTPUT
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     Test for user-specified or auto-detected input RECL
C     ------------------------------------------------------------------
C
      CALL LDFOOK(IERR)          !Tst for ldf-output file write-ready 
      IF(IERR.NE.0) RETURN       !If not, RETURN
C
      RECTST='    '
      TRECL=32768
C
      IF(LAUTO.NE.'YES ') THEN
      RECTST='DONE'
      TRECL=INRECL
      WRITE(CMSSG,10)TRECL
   10 FORMAT('Using user-specified input tape RECL =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      ENDIF
C
      TRECLFW=TRECL/4
      JDXMAX=8192-TRECLFW
C
      JDX=0
      NBYTF=32768
      NFWF =NBYTF/4
C
      IERR=0
      MSGX=0
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
C
      IF(LINO.NE.'YES ') GO TO 1020
C
      IF(KMD.EQ.'CC  ')  GO TO 100
C
C     ------------------------------------------------------------------
C     Process - COPY N(files)
C          or - CREC N(records)
C     ------------------------------------------------------------------
C 
      NFILE=1
      NREC=100000000
      IF(KMD.EQ.'CREC') NREC=1
      IF(NFE.EQ.1)        GO TO 50
      IF(NFE.NE.2)        GO TO 1010
      CALL IVALU(LWD(1,2),IV,IERR)
      IF(IERR.NE.0)       GO TO 1010
      IF(IV.LT.1)         GO TO 1010
      IF(KMD.EQ.'COPY') NFILE=IV
      IF(KMD.EQ.'CREC') NREC =IV
C
   50 NFCOP=0
      NRCOP=0
      NRTF=0
C
C     ------------------------------------------------------------------
C     Read a record from the tape
C     ------------------------------------------------------------------
C
  100 IF(MSGF.NE.'    ') THEN
                         MSGF='    '
                         MSGX=1
                         GO TO 550
                         ENDIF
C
      CALL READUM1(LUCI,IBUF,TRECL,NBYT,STAT)
      IF(STAT.NE.'GOOD') GO TO 600
C
C     ------------------------------------------------------------------
C     Good status - check for byte-swap, header-record, etc
C     ------------------------------------------------------------------
C
      IF(ISWAB.EQ.'YES ') CALL SWAPPER(IBUF,NBYT)
      IF(ISWAH.EQ.'YES ') CALL SWAPHED(IBUF,NBYT)
C
      NEOF=0
      IF(NBYT.NE.256)    GO TO 110
      WRITE(CMSSG,105)(IBUF(I),I=9,23),IBUF(33)
      CALL MESSLOG(LOGUT,LOGUP)
  105 FORMAT(15A4,' - #',I12)
C
C     ------------------------------------------------------------------
C     Check for auto-recl required
C     ------------------------------------------------------------------
C
  110 IF(RECTST.EQ.'DONE') GO TO 200
C
      IF(NBYT.EQ.256)      GO TO 540
      IF(NBYT.EQ.128)      GO TO 540
      IF(NBYT.LT.2048)     GO TO 540
      IF(NBYT.EQ.32000)    GO TO 540
      IF(MOD(NBYT,2).NE.0) GO TO 100
C
      TRECL=NBYT
      TRECLFW=TRECL/4
      JDXMAX=8192-TRECLFW
      RECTST='DONE'
C
      WRITE(CMSSG,115)TRECL
  115 FORMAT('Using auto-detected input tape RECL =',I6)
      CALL MESSLOG(LOGUT,LOGUP)
C
C     ------------------------------------------------------------------
C     Check for and output data-record
C     ------------------------------------------------------------------
C
  200 IF(NBYT.NE.TRECL) GO TO 540
C
      IF(IBUF(1).EQ.SCAL) THEN
      DO 210 I=TRECLFW+1,8192
      IBUF(I)=BLANK
  210 CONTINUE
      NBYT=32000
      GO TO 540
      ENDIF
C
      DO 520 I=1,TRECLFW
      JDX=JDX+1
      JBUF(JDX)=IBUF(I)
  520 CONTINUE
      NRCOP=NRCOP+1
      NRTF =NRTF +1
      IF(JDX.LE.JDXMAX) GO TO 100
      DO 530 I=JDX+1,NFWF
      JBUF(I)=-1
  530 CONTINUE
C
      CALL LDFWRIT(LUOUF,'DATA',NBYTF,JBUF,IERR)
      JDX=0
      IF(IERR.NE.0)     GO TO 1000
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
      IF(NRCOP.LT.NREC) GO TO 100
      GO TO 550
C
C     ------------------------------------------------------------------
C     Output other kinds of records
C     ------------------------------------------------------------------
C
  540 MODE='    '
C
      IF(JDX.GT.0) THEN
      DO 545 I=JDX+1,NFWF
      JBUF(I)=-1
  545 CONTINUE
      CALL LDFWRIT(LUOUF,'DATA',NBYTF,JBUF,IERR)
      LRIT='DATA'
      JDX=0
      ENDIF
C
      IF(NBYT.EQ.32000) MODE='SCAL'
      IF(NBYT.EQ.1600)  MODE='PAC '
      IF(NBYT.EQ.256)   MODE='HEAD'
      IF(NBYT.EQ.128)   MODE='DEAD'
      IF(MODE.EQ.'   ') GO TO 100
C
      CALL LDFWRIT(LUOUF,MODE,NBYT,IBUF,IERR)
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
      IF(IERR.NE.0) GO TO 550
C
      NRCOP=NRCOP+1
      NRTF=NRTF+1
      IF(NRCOP.LT.NREC)  GO TO 100
C
C     ------------------------------------------------------------------
C     We get her via CTRL/C or specified # records copied
C
C     Report #files & #records copied & write EOFs
C     ------------------------------------------------------------------
C
  550 WRITE(CMSSG,555)NRTF,NRCOP
      CALL MESSLOG(LOGUT,LOGUP)
  555 FORMAT('RECS-THIS-FILE, TOTAL-RECS COPIED =',2I8)
  560 FORMAT('NUMBER OF            FILES COPIED =',I8)
C
      IF(JDX.GT.0) THEN
      DO 565 I=JDX+1,NFWF
      JBUF(I)=-1
  565 CONTINUE
      CALL LDFWRIT(LUOUF,'DATA',NBYTF,JBUF,IERR)
      JDX=0
      ENDIF
C
      CALL LDFILMAR(2,2,IERR)
C
      LRIT='EOF '                          !SET LAST-RIT-FLG='DATA'
      IF(IERR.NE.0)      GO TO 1000
      IF(STAT.NE.'GOOD') GO TO 1000
      IF(MSGX.NE.0)      GO TO 1000
      RETURN
C
C     ------------------------------------------------------------------
C     We get here via input stat = EOF or "BAD" - write EOFs on output
C     ------------------------------------------------------------------
C
  600 WRITE(CMSSG,555)NRTF,NRCOP
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(STAT.EQ.'EOF ')  GO TO 610
      IF(STAT.EQ.'EOT ')  GO TO 610
      IF(STAT.EQ.'EOV ')  GO TO 610
C
      IF(JDX.GT.0) THEN
      DO 605 I=JDX+1,NFWF
      JBUF(I)=-1
  605 CONTINUE
      CALL LDFWRIT(LUOUF,'DATA',NBYTF,JBUF,IERR)
      JDX=0
      ENDIF
C
      IF(LRIT.EQ.'EOF ') CALL LDFILMAR(1,1,IERR)
      IF(LRIT.NE.'EOF ') CALL LDFILMAR(2,1,IERR)
C
      LRIT='EOF '                          !SET LAST-RIT-FLG='DATA'
      GO TO 1000
C
C
C     ------------------------------------------------------------------
C     We get here via input stat = EOF  - write EOFs on output
C     ------------------------------------------------------------------
C
  610 NEOF=NEOF+1
C
      IF(JDX.GT.0) THEN
      DO 615 I=JDX+1,NFWF
      JBUF(I)=-1
  615 CONTINUE
      CALL LDFWRIT(LUOUF,'DATA',NBYTF,JBUF,IERR)
      JDX=0
      ENDIF
C
      IF(LRIT.EQ.'EOF ') CALL LDFILMAR(1,1,IERR)
      IF(LRIT.NE.'EOF ') CALL LDFILMAR(2,1,IERR)
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
      CALL TAPCLEARI(JERR)
      IF(JERR.NE.0)GOTO 1000
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
C     Error return
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
 1025 FORMAT('INPUT TAPE/FILE NOT ASSIGNED - COMMAND IGNORED')
      GO TO 1200
C
 1200 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
