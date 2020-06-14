C$PROG SCAD      - Scaler display with tabular, graphic & alarm features
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/18/2004
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
      COMMON/III/  LIN,LCM,LCI,LOU
      INTEGER*4    LIN,LCM,LCI,LOU
C     ------------------------------------------------------------------
      COMMON/HEP/  IHEPF
      CHARACTER*4  IHEPF
C     ------------------------------------------------------------------
      CHARACTER*4  CLWD(2,40),CIWD(20),IDONE,KMD
C
      EQUIVALENCE (CIWD,IWD),(CLWD,LWD)
C
      INTEGER*4    IERR,I
C
      EQUIVALENCE (KMD,LWD(1,1))
C
      DATA LIN,LCI,LCM,LOU/5,5,9,17/
C
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL SCADMSG
C
      CALL SCADNIT
C
      GO TO 100
C
   50 LIN=LCI
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 IF(LIN.EQ.LCI) WRITE(LOGUT,105)
  105 FORMAT('SCAD->',$)
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
      IF(CIWD(1).EQ.'COM ') GO TO 130
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
      IF(CLWD(2,1).NE.'    ') GO TO 1000
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
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 08/07/98
C     ******************************************************************
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      SAVE
C
C
      IF(IDONE.NE.'YES ') CALL CMPSCAD(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPSCUD(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') CALL CMPRUN(IDONE,IERR)
C
C
      RETURN
      END
