C$PROG RECLOGLDF - Logs/Disps aux (non-data) records from LDF-files
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE RECLOGLDF(IERR)
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
      COMMON/LM25/ LUREC,OFLG
      INTEGER*4    LUREC
      CHARACTER*4        OFLG
C     ------------------------------------------------------------------
      CHARACTER*4  KMD,STAT,KIND,LTYPE,KMDL(4,2),LFLG(4)
C
      INTEGER*4    IDNUM,IERR,NEOF,JEOF,JEOFM,JREC
C
      INTEGER*4    LU,NREC,NUMBY,NFW,NDO,IA,IB,I,J
C
      INTEGER*4     LISTF(16384)
      CHARACTER*4  CLISTF(16384)
      EQUIVALENCE (CLISTF,LISTF)
C
      EQUIVALENCE (KMD,LWD(1,1)),
     &            (KMDL(1,1),LWD(1,2)),
     &            (LISTF(1),IBUF(1))
C
      SAVE
C
C     ------------------------------------------------------------------
C     Logs/Displays auxillary records (non-data) from LDF-files
C     ------------------------------------------------------------------
C
      IERR=0                            !RESET ERROR FLAG
C
      IF(KMD.EQ.'LISI') LU=LUINF
C
      IF(KMD.EQ.'LISO') LU=LUOUF
C
      STAT='GOOD'                       !RESET STATUS
      NEOF=0                            !RESET EOF-COUNTER
      JEOF=0                            !RESET CURRENT-FILE COUNTER
      JREC=0                            !RESET CURRENT-REC  COUNTER
      LTYPE='    '                      !RESET LAST-RECORD  TYPE
C
      LFLG(1)='DEAD'
      LFLG(2)='HEAD'
      LFLG(3)='PAC '
      LFLG(4)='SCAL'
C
      IF(NTER.NE.0) GO TO 1100
      IF(NF.LT.1)   GO TO 1100
      IF(NF.GT.5)   GO TO 1100
      IF(NF.EQ.1)   GO TO 50
C
      DO 20 J=1,4
      DO 10 I=1,NF-1
      IF(KMDL(I,1).EQ.LFLG(J)) GO TO 20
   10 CONTINUE
      LFLG(J)='    '
   20 CONTINUE
C
   50 IF(MSGF.NE.'    ') GO TO 1100     !TST FOR STOP MESSAGE
      IF(NEOF.LT.2)      GO TO 70       !TST FOR DOUBLE-EOF
C
      WRITE(CMSSG,55)JEOF               !IF YES, SAY SO
      CALL MESSLOG(LOGUT,LOGUP)
   55 FORMAT('DOUBLE END-OF-FILE ENCOUNTERED AT FILE-MARK OFFSET',I6)
C
      IF(KMD.EQ.'LISI') INRECI=INRECI-1
      IF(KMD.EQ.'LISO') OURECI=OURECI-1
C
      JEOFM=JEOF-1
      WRITE(CMSSG,60)JEOFM,JEOF
   60 FORMAT('WITH LUCK - I AM BETWEEN FILE-MARK OFFSETS',I5,'  &',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2000                        !TAKE ERROR RETURN
C
   70 IF(KMD.EQ.'LISI') THEN
                        INRECI=INRECI+1
                        NREC=INRECI
                        ENDIF
C
      IF(KMD.EQ.'LISO') THEN
                        OURECI=OURECI+1
                        NREC=OURECI
                        ENDIF
C
      CALL LDFREAD(LU,NREC,LISTF,KIND,NFW,STAT)
C
      NUMBY=4*NFW
C
      IF(STAT.EQ.'GOOD') GO TO 80       !TST FOR GOOD STATUS
C
      IF(STAT.EQ.'EOF ') THEN           !TST FOR END-OF-FILE
                         NEOF=NEOF+1    !INC N-FILE    COUNTER
                         JEOF=JEOF+1    !INC FILE-MARK COUNTER
                         JREC=0         !ZOT RECORD    COUNTER
                         GO TO 50
                         ENDIF
      GO TO 1120                        !OTHERWISE, TAKE ERROR RETN
C
   80 NEOF=0                            !RESET EOF-COUNTER
      JREC=JREC+1
C
      IF(KIND.EQ.'DEAD') GO TO 100
      IF(KIND.EQ.'HEAD') GO TO 200
      IF(KIND.EQ.'PAC ') GO TO 300
      IF(KIND.EQ.'SCAL') GO TO 400
C
      IF(CLISTF(1).EQ.'SCAL') GO TO 400
C
      LTYPE='DATA'
      GO TO 50
C
  100 IF(LFLG(1).EQ.'    ') GO TO 120
      IF(LTYPE.NE.'DEAD') THEN
      WRITE(CMSSG,105)LFLG(1),JEOF+1,JREC
  105 FORMAT(A4,12('----'),'FILE#,REC# =',I4,I8)
      CALL LISLOG(LUREC,OFLG)
      ENDIF
      WRITE(CMSSG,110)(LISTF(I),I=1,20)
  110 FORMAT(20A4)
      CALL LISLOG(LUREC,OFLG)
  120 LTYPE='DEAD'
      GO TO 50
C
  200 IF(LFLG(2).EQ.'    ') GO TO 220
      IF(LTYPE.NE.'HEAD') THEN
      WRITE(CMSSG,105)LFLG(2),JEOF+1,JREC
      CALL LISLOG(LUREC,OFLG)
      ENDIF
      IF(ISWAH.EQ.'YES ') CALL SWAPHED(LISTF,256)
      IDNUM=LISTF(33)
      IF(ISWAB.EQ.'YES ') CALL SWAPF(IDNUM,1,1)
      WRITE(CMSSG,210)(LISTF(I),I=9,23),IDNUM
      CALL LISLOG(LUREC,OFLG)
  210 FORMAT(15A4,I12)
  220 LTYPE='HEAD'
      GO TO 50
C
  300 IF(LFLG(3).EQ.'    ') GO TO 320
      IF(LTYPE.NE.'PAC ') THEN
      WRITE(CMSSG,105)LFLG(3),JEOF+1,JREC
      CALL LISLOG(LUREC,OFLG)
      ENDIF
      IB=0
      NDO=NFW/20
      DO 310 J=1,NDO
      IA=IB+1
      IB=IA+19
      WRITE(CMSSG,305)(LISTF(I),I=IA,IB)
  305 FORMAT(20A4)
      CALL LISLOG(LUREC,OFLG)
  310 CONTINUE
  320 LTYPE='PAC '
      GO TO 50
C
  400 IF(LFLG(4).EQ.'    ') GO TO 420

      IF (LTYPE.NE.'SCAL') THEN
         WRITE(CMSSG,105)LFLG(4),JEOF+1,JREC
         CALL LISLOG(LUREC,OFLG)
      ENDIF

      IB=0
      DO 410 J=1,40
         IA=IB+1
         IB=IA+19
         WRITE(CMSSG,405)(LISTF(I),I=IA,IB)
  405    FORMAT(20A4)
         CALL LISLOG(LUREC,OFLG)
         IF(CLISTF(IA).EQ.'    ') GO TO 50
  410 CONTINUE

  420 LTYPE='SCAL'
      GO TO 50
C
C     ------------------------------------------------------------------
C     ERROR RETURN - REQUESTED HEADER-ID NOT FOUND
C     ------------------------------------------------------------------
C
 1100 WRITE(CMSSG,1105)
 1105 FORMAT('PROCESS INTERRUPTED VIA CTRL-C')
      GO TO 1200
C
 1120 WRITE(CMSSG,1125)STAT
 1125 FORMAT('HEDLOC ERROR READING - STAT =',A4)
      GO TO 1200
C
 1200 CALL MESSLOG(LOGUT,LOGUP)
C
 2000 IERR=1
      RETURN
      END
