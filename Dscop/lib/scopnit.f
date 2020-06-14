C$PROG SCOPNIT   - Initializing routine for SCOP
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/25/2000
C     ******************************************************************
C
      SUBROUTINE SCOPNIT(LHEP)
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
      COMMON/HEP/  IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI,LOU
      INTEGER*4    LIN,LCM,LCI,LOU
C     ==================================================================
      COMMON/XLAA/ DPY,WDID(20),XN(20),YN(20),NUWIN(20),WN
      INTEGER*4    DPY,WDID                                      !STAR-8
      INTEGER*4                 XN,    YN,    NUWIN,    WN
C     ------------------------------------------------------------------
      COMMON/DD03/ NCALLR
      INTEGER*4    NCALLR
C     ------------------------------------------------------------------
      COMMON/DD11/ RATFLG,   RATAVG(9),   RATPAR(9),   RATTYP(9),
     &             RATDSP,   NRATE
C
      INTEGER*4              RATAVG,      RATPAR,      NRATE
      CHARACTER*4  RATFLG,   RATDSP,      RATTYP
C     ------------------------------------------------------------------
      COMMON/DD13/ CC(16),NN(16),AA(16),FF(16),LAG(3,16),NSCA
      INTEGER*4    CC,    NN,    AA,    FF,    LAG,      NSCA
C     ------------------------------------------------------------------
      INTEGER*4    NCON,IOS,LHEP,IERR,STAT,LU
C
      INTEGER*4    IARGC,STRAPPEND,STRLEN,NUMARG,NLOST
C
      INTEGER*4    NAMHEP(6)
C
      CHARACTER*4  KMD,SNITDUN
C
      CHARACTER*24 CNAMHEP
C
      CHARACTER*80 CWD,CWDRAW,ARG
C
      EQUIVALENCE (CNAMHEP,NAMHEP)
C
      EQUIVALENCE (CWDRAW,IWDRAW)
C
      DATA         LU/1/
C
      DATA         NAMPROG/'SCOP','    '/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CMSSG=' '
C
      CNAMHEP='scop.hep'
C
      MSGF='    '
      LISFLG='LON '
      LOGUT=6
      LOGUP=7
C
      CALL COMNIT
C
C     ------------------------------------------------------------------
C     Open log-file, help-file, etc
C     ------------------------------------------------------------------
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'scop.log',
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = IOS)
C
      CLOSE(UNIT=LOGUP,DISP='DELETE')
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'scop.log',
     &     STATUS     = 'NEW',
     &     IOSTAT     = IOS)
C
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   LOGUP=0
                   WRITE(LOGUT,160)
                   ENDIF
C
  160 FORMAT(1H ,'OUTPUT TO LOG-FILE IS DISABLED')
C
C     ------------------------------------------------------------------
C     Pick up start string (snit file name) if any
C     ------------------------------------------------------------------
C
      SNITDUN='NO  '
      NUMARG=IARGC()
      IF(NUMARG.EQ.1) THEN
      CALL GETARG(1,ARG)
      CWDRAW='SNIT '
      NLOST=STRAPPEND(CWDRAW,ARG(1:STRLEN(ARG)))
      CALL SNITTER(IERR)
      IF(IERR.EQ.0) SNITDUN='YES '
      ENDIF
C
C     ------------------------------------------------------------------
C
      CALL BEAMLINE(IERR)
C
      IF(IERR.NE.0) STOP
C
      CALL HELPOPEN(LHEP,NAMHEP,IHEPF)
C
      IF(IHEPF.NE.'YES ') GO TO 230
C
      WRITE(LOGUT,200)
      WRITE(LOGUT,210)
      WRITE(LOGUT,220)
  200 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
  210 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
  220 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
  230 CALL CTCNIT
C
      IF(SNITDUN.EQ.'YES ') GO TO 250
C
      CWDRAW='SNIT operator.sca'
C
      CALL SNITTER(IERR)
C
      IF(IERR.NE.0) GO TO 260
C
  250 WRITE(CWD,255)NSCA
  255 FORMAT('FIG ',I1)
C
      CALL NEWFIG(LU,CWD,IERR)
      KMD='FIGG'
      CALL XX_WINMAN(KMD,0)
      CALL DOMETER('INIT',0,0.0)
C
      CALL WINRAT
C
      CALL XX_SYNC(DPY,.TRUE.)
C
  260 NCALLR=0
C
      RETURN
C
      END
