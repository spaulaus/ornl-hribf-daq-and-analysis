C$PROG TEXSAVLDF - Find and stores PAC-file records - from ldf-file
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE TEXSAVLDF(IERR)
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
      COMMON/LM20/ LUINF,LUOUF,INFOP,OUFOP
      INTEGER*4    LUINF,LUOUF
      CHARACTER*4              INFOP,OUFOP
C     ------------------------------------------------------------------
      COMMON/LM23/ INDIR(8192),OUDIR(8192),INTYP,OUTYP,INRECI,OURECI
      INTEGER*4    INDIR,      OUDIR,                  INRECI,OURECI
      CHARACTER*4                          INTYP,OUTYP
C     ------------------------------------------------------------------
      CHARACTER*4  STAT,KIND,FOUND
C
      INTEGER*4    STRLEN,NXNB,LSNB
C
      INTEGER*4    IERR,NEOF,JEOF,JEOFM,JREC,N
C
      INTEGER*4    LINE(20),NREC,NUMBY,NFW,IA,IB,I,J,ISTAT,NDO
C
      CHARACTER*80 CIWD,CLINE
C
      EQUIVALENCE (CIWD,IWDRAW),(CLINE,LINE)
C
      SAVE
C
C     ------------------------------------------------------------------
C     CREATE A FILE FOR STORAGE OF TEXT (PAC file)
C     ------------------------------------------------------------------
C
      IERR=0
C
      CLOSE(UNIT=LTX)
      IA=NXNB(IWDRAW,5,80)
      IF(IA.LE.0) GO TO 900
      IB=LSNB(IWDRAW,IA,80)
      IF(IB.LE.0) GO TO 900
C
      OPEN(UNIT       = LTX,
     &     FILE       = CIWD(IA:IB),
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = ISTAT)
C
      IF(ISTAT.NE.0) THEN
                     CALL IOFERR(ISTAT)
                     GO TO 910
                     ENDIF
C
C     ------------------------------------------------------------------
C     Logs/Displays auxillary records (non-data) from LDF-files
C     ------------------------------------------------------------------
C
      IERR=0                            !RESET ERROR FLAG
      STAT='GOOD'                       !RESET STATUS
      NEOF=0                            !RESET EOF-COUNTER
      JEOF=0                            !RESET CURRENT-FILE COUNTER
      JREC=0                            !RESET CURRENT-REC  COUNTER
      FOUND='NO  '                      !Reset found flag
C
   50 IF(MSGF.NE.'    ') GO TO 1100     !TST FOR STOP MESSAGE
      IF(NEOF.LT.2)      GO TO 70       !TST FOR DOUBLE-EOF
C
      WRITE(CMSSG,55)JEOF               !IF YES, SAY SO
      CALL MESSLOG(LOGUT,LOGUP)
   55 FORMAT('DOUBLE END-OF-FILE ENCOUNTERED AT FILE-MARK OFFSET',I6)
C
      INRECI=INRECI-1
C
      JEOFM=JEOF-1
      WRITE(CMSSG,60)JEOFM,JEOF
   60 FORMAT('WITH LUCK - I AM BETWEEN FILE-MARK OFFSETS',I5,'  &',I6)
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 2000                        !TAKE ERROR RETURN
C
   70 INRECI=INRECI+1
      NREC=INRECI
C
      CALL LDFREAD(LUINF,NREC,IBUF,KIND,NFW,STAT)
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
      IF(KIND.EQ.'PAC ')  GO TO 300
C
      IF(FOUND.EQ.'NO  ') GO TO 50
C
      GO TO 2100
C
C
  300 FOUND='YES '
      IB=0
      NDO=NFW/20
      DO 350 J=1,NDO
      CLINE=' '
      IA=IB+1
      IB=IA+19
      N=0
      DO 310 I=IA,IB
      N=N+1
      LINE(N)=IBUF(I)
  310 CONTINUE
      WRITE(LTX,  315)CLINE(1:STRLEN(CLINE))
      WRITE(LOGUT,320)CLINE(1:STRLEN(CLINE))
  315 FORMAT(A)
  320 FORMAT(1H ,A)
  350 CONTINUE
      GO TO 50
C
C     ------------------------------------------------------------------
C     ERROR RETURNS
C     ------------------------------------------------------------------
C
  900 WRITE(CMSSG,905)
  905 FORMAT('SYNTAX ERROR IN FILENAME SPECIFICATION')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 1200
C
  910 WRITE(CMSSG,915)
  915 FORMAT('ERROR OPENING TEX-FILE')
      CALL MESSLOG(LOGUT,LOGUP)
      GO TO 1200
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
C
 2100 CLOSE(UNIT=LTX)
C
      RETURN
      END
