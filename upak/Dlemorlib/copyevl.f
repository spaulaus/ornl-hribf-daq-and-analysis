C$PROG COPYEVL   - Copies simulated event-files to tape
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE COPYEVL(KMD,NFE,IERR)
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
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD,STAT,LRIT
C
      INTEGER*4    NFE,IERR,IV,I
C
      INTEGER*4    JBUF(8192),ERECL,ERECLFW,NBYTF,NFWF,JDX,JDXMAX
C
      INTEGER*4    MSGX,NFILE,NREC,NFCOP,NRCOP,NRTF,NBYT,NEOF
C
      DATA         NEOF,NRTF,NRCOP,NFCOP,NREC,NFILE/6*0/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     COPY N (FILES) OR CREC N (RECS) FROM EVL-FILE TO OUTPUT-TAPE-1
C     ------------------------------------------------------------------
C
      IERR=0
      MSGX=0
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
C
      IF(INTYP.NE.'EVEL')   GO TO 1020
C
      IF(OUTYP.EQ.'TAPE'.OR.
     &   OUTYP.EQ.'LDF ')   GO TO 100
C
      GO TO 1030
C
  100 IF(KMD.EQ.'CC  ')     GO TO 500
C
      ERECL=LNBY
      ERECLFW=ERECL/4
      JDXMAX=8192-ERECLFW
      JDX=0
      NBYTF=32768
      NFWF=NBYTF/4
C
      NFILE=1
      NREC=100000000
      IF(KMD.EQ.'CREC') NREC=1
      IF(NFE.EQ.1)      GO TO 490
      IF(NFE.NE.2)      GO TO 1010
      CALL IVALU(LWD(1,2),IV,IERR)
      IF(IERR.NE.0)     GO TO 1010
      IF(IV.LT.1)       GO TO 1010
      IF(KMD.EQ.'COPY') NFILE=IV
      IF(KMD.EQ.'CREC') NREC =IV
C
  490 NFCOP=0
      NRCOP=0
      NRTF=0
C
  500 IF(MSGF.NE.'    ') THEN
                         MSGF='    '
                         MSGX=1
                         GO TO 550
                         ENDIF
C
      CALL READUM1(LUCI,IBUF,65536,NBYT,STAT)
      IF(STAT.NE.'GOOD') GO TO 600
C
      IF(ISWAB.EQ.'YES ') CALL SWAPPER(IBUF,NBYT)
C
      NEOF=0
C
      IF(OUTYP.EQ.'TAPE') THEN
      CALL WRITUM(LUCO(1),IBUF,NBYT,STAT)
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
      IF(STAT.NE.'GOOD') GO TO 550
      NRCOP=NRCOP+1
      NRTF=NRTF+1
      GO TO 540
      ENDIF
C
      IF(OUTYP.EQ.'LDF ') THEN
      DO 520 I=1,ERECLFW
      JDX=JDX+1
      JBUF(JDX)=IBUF(I)
  520 CONTINUE
      NRCOP=NRCOP+1
      NRTF =NRTF +1
      IF(JDX.LE.JDXMAX) GO TO 500
      DO 530 I=JDX+1,NFWF
      JBUF(I)=-1
  530 CONTINUE
      CALL LDFWRIT(LUOUF,'DATA',NBYTF,JBUF,IERR)
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
      JDX=0
      IF(IERR.NE.0)      GO TO 550
      GO TO 540
      ENDIF
C
  540 IF(NRCOP.LT.NREC)  GO TO 500
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
      CALL FILMAR(LUCO(1),2,2,IERR)
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
      IF(IERR.NE.0)      GO TO 1000
      IF(STAT.NE.'GOOD') GO TO 1000
      IF(MSGX.NE.0)      GO TO 1000
      RETURN
C
  600 WRITE(CMSSG,555)NRTF,NRCOP
      CALL MESSLOG(LOGUT,LOGUP)
C
      IF(JDX.GT.0) THEN
      DO 615 I=JDX+1,NFWF
      JBUF(I)=-1
  615 CONTINUE
      CALL LDFWRIT(LUOUF,'DATA',NBYTF,JBUF,IERR)
      JDX=0
      ENDIF
C
      IF(STAT.EQ.'EOF ')  GO TO 610
      IF(STAT.EQ.'EOT ')  GO TO 610
      IF(STAT.EQ.'EOV ')  GO TO 610
C
      CALL FILMAR(LUCO(1),2,2,IERR)
      LRIT='DATA'                          !SET LAST-RIT-FLG='DATA'
      GO TO 1000
C
  610 NEOF=NEOF+1
      IF(LRIT.NE.'EOF ') CALL FILMAR(LUCO(1),2,1,IERR)
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
  620 IF(NEOF.LT.2)      GO TO 500
      RETURN
C
  640 FORMAT('DOUBLE END-OF-FILE ENCOUNTERED')
C
 1000 IERR=1
      RETURN
C
 1010 WRITE(CMSSG,1015)
 1015 FORMAT('SYNTAX ERROR OR ILLEGAL VALUE - IGNORED')
      GO TO 1200
C
 1020 WRITE(CMSSG,1025)
 1025 FORMAT('INPUT EVEL-FILE NOT ASSIGNED - COMMAND IGNORED')
      GO TO 1200
C
 1030 WRITE(CMSSG,1035)
 1035 FORMAT('OUTPUT TAPE NOT ASSIGNED - COMMAND IGNORED')
C
 1200 CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
