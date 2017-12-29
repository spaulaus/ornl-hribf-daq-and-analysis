C$PROG SCADNIT   - Initializing routine for SCAD
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 05/22/2005
C     ******************************************************************
C
      SUBROUTINE SCADNIT
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
      COMMON/HEP/  IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      COMMON/SD11/ MAXR, MAXC, MAXD, MAXT, MAXS
      INTEGER*4    MAXR, MAXC, MAXD, MAXT, MAXS
C
      DATA         MAXR, MAXC, MAXD, MAXT, MAXS/24,80,0,500,50/
C     ------------------------------------------------------------------
      COMMON/SD07/ KMD,SEC,LSEC
      CHARACTER*4  KMD
      INTEGER*4        SEC,LSEC
C     ------------------------------------------------------------------
      COMMON/SD13/ ISET,LULG
      CHARACTER*4  ISET
      INTEGER*4         LULG
C
      DATA              LULG/15/
C     ------------------------------------------------------------------
      COMMON/SD15/ LUAL,THUSH,TNOW,ALFLG
      INTEGER*4    LUAL,THUSH,TNOW
      CHARACTER*4                  ALFLG
C     ------------------------------------------------------------------
      COMMON/SCD1/ GNITNAM
      CHARACTER*80 GNITNAM
C
      DATA         GNITNAM/'Undefined'/
C     ------------------------------------------------------------------
C
      CHARACTER*4  CIWD(20)
      EQUIVALENCE (CIWD,IWD)
C
      character*4  cNAMPROG(2)
      equivalence  (cNAMPROG, NAMPROG)
      DATA         cNAMPROG/'scad','    '/
C
      INTEGER*4    IERR,STAT,IOS,LHEP
C
      CHARACTER*24 NAMHEP
C
      DATA         LHEP/16/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C
      CMSSG=' '
C
      NAMHEP='scad.hep'
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
     &     FILE       = 'scad.log',
     &     STATUS     = 'UNKNOWN',
     &     IOSTAT     = IOS)
C
      CLOSE(UNIT=LOGUP,STATUS='DELETE')
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'scad.log',
     &     STATUS     = 'NEW',
     &     IOSTAT     = IOS)
C
C
      IF(IOS.NE.0) THEN
                   CALL IOFERR(IOS)
                   LOGUP=0
                   WRITE(LOGUT,20)
                   ENDIF
C
   20 FORMAT(1H ,'OUTPUT TO LOG-FILE IS DISABLED')
C
C
C     ------------------------------------------------------------------
C     Open alarm file
C     ------------------------------------------------------------------
C
      LUAL=11
      THUSH=0
      TNOW=0
      ALFLG='OFF '
      OPEN(UNIT   = LUAL,
     &     FILE   = 'scadalarm.dat',
     &     STATUS = 'UNKNOWN',
     &     RECL   = 12,
     &     ACCESS = 'DIRECT',
     &     IOSTAT = STAT)
C
      WRITE(LUAL,REC=1)TNOW
      CALL FLUSH(LUAL)
C
C     ------------------------------------------------------------------
C     Pick up snit-file name for tabular display
C     ------------------------------------------------------------------
C
   50 WRITE(6,55)
   55 FORMAT('Enter snit-filename for tabular display->',$)
      READ(5,60)IWD
   60 FORMAT(20A4)
      KMD=CIWD(1)
C
      IF(KMD.EQ.'end ') CALL EXIT
      IF(KMD.EQ.'END ') CALL EXIT
C
      CALL LOGOPEN(LULG,IWD,IERR)
      IF(IERR.NE.0) GO TO 50
C
      CALL CTCNIT
C
      CALL CTBSNIT
C
      KMD='NORT'
      CALL NORMAN
C
      ISET='NEW '
C
      CALL SETUP(ISET)
C
C     ------------------------------------------------------------------
C     Open helpfile
C     ------------------------------------------------------------------
C
      CALL HELPOPEN(LHEP,NAMHEP,IHEPF)
C
      IF(IHEPF.NE.'YES ') RETURN
C
      WRITE(LOGUT,100)
      WRITE(LOGUT,110)
      WRITE(LOGUT,120)
  100 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
  110 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
  120 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
C
      RETURN
      END
