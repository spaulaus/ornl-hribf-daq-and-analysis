C$PROG STOPE     - General Stopping Power program (graphical version)
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
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
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/III/  LIN,LCM,LCI
      INTEGER*4    LIN,LCM,LCI
C     ------------------------------------------------------------------
C
      CHARACTER*4  IDONE,KMD,KMI,EXX
C
      INTEGER*4    IERR,I
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMI,IWD),(EXX,LWD(2,1))
C
      DATA LIN,LCI,LCM/5,5,9/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL STOPENIT
C
      GO TO 100
C
   50 LIN=LCI
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 IF(LIN.EQ.LCI) WRITE(LOGUT,105)
  105 FORMAT(' STOPE->',$)
      READ(LIN,110,END=50,ERR=50)IWD
  110 FORMAT(20A4)
      MSGF='    '
C
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
  115 CONTINUE
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      IF(KMI.EQ.'    ') GO TO 100
      IF(KMI.EQ.'COM ') GO TO 130
      GO TO 140
C
  130 IF(LIN.NE.LCI) THEN
                     WRITE(CMSSG,110)IWD
                     CALL MESSLOG(LOGUT,LOGUP)
                     GO TO 100
                     ENDIF
C
      WRITE(CMSSG,110)IWD
      CALL MESSLOG(0,LOGUP)
      GO TO 100
C
  140 WRITE(CMSSG,110)IWD
      IF(LIN.EQ.LCI) CALL MESSLOG(0,LOGUP)
      IF(LIN.NE.LCI) CALL MESSLOG(LOGUT,LOGUP)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(EXX.NE.'    ') GO TO 1000
C
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPER(LIN,LCI)
                        GO TO 100
                        ENDIF
C
      IDONE='    '
      CALL CALLER(IDONE,IERR)
      IF(IDONE.EQ.'YES ') GO TO 100
C
      WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      GO TO 1500
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax Error - Command Ignored')
C
 1500 CALL MESSLOG(LOGUT,LOGUP)
      GO TO 100
      END
C$PROG CALLER    - Routine caller
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL EQUATE(IDONE,IERR)
C   
      IF(IDONE.NE.'YES ') CALL MODFIN(IERR)
      IF(IERR.NE.0)       RETURN
C   
      IF(IDONE.NE.'YES ') CALL LWDMOD
C
      IF(IDONE.NE.'YES ') CALL CMPGEN(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPDEDX(IDONE,IERR)
C
C
      RETURN
      END
C$PROG STOPENIT   - Initializing routine for program STOPE
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE STOPENIT
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
      character*4 cnamprog(2)
      equivalence (cnamprog,namprog)
      DATA        cNAMPROG/'STOP','E   '/
C
      INTEGER*4    I
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CMSSG=' '
      MSGF='    '
      LISFLG='LON '
      LOGUT=6
      LOGUP=7
C
      OPEN(UNIT       =1,
     &     FILE       ='/usr/hhirf/stope.dat',
     &     STATUS     ='OLD',
     &     ACCESS     ='SEQUENTIAL')
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'stope.log',
     &     STATUS     = 'UNKNOWN')
C
c     CLOSE(UNIT=LOGUP,DISP='DELETE')
      CLOSE(UNIT=LOGUP)
C
      OPEN(UNIT       = LOGUP,
     &     FILE       = 'stope.log',
     &     STATUS     = 'REPLACE')
C
      CALL HELPNIT(IHEPF)
C
      CALL CTCNIT
C
      RETURN
C
      END
C$PROG HELPNIT   - Opens stope.hep
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE HELPNIT(IHEPF)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      CHARACTER*4  IHEPF
C
      INTEGER*4    NAMHEP(6),LHEP
C
      CHARACTER*24 CNAMHEP
C
      EQUIVALENCE (CNAMHEP,NAMHEP)
C
      DATA         LHEP/13/
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CNAMHEP='stope.hep'
C
      CALL HELPOPEN(LHEP,NAMHEP,IHEPF)
C
      IF(IHEPF.NE.'YES ') RETURN
C
      WRITE(6,30)
      WRITE(6,35)
      WRITE(6,40)
   30 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
   35 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
   40 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
      RETURN
      END
