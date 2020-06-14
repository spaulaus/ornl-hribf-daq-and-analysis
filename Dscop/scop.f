C$PROG SCOP      - Scaler Rate Manager (control program)
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/31/2001
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
      COMMON/HEP/  IHEPF
      CHARACTER*4  IHEPF
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
      INTEGER*4    IERR,LHEP,I
C
      CHARACTER*4  KMD,KMX,KMI,IDONE
C
      EQUIVALENCE (KMD,LWD(1,1)),(KMX,LWD(2,1)),(KMI,IWD(1))
C
      DATA         LIN,LCI,LCM,LHEP,LOU/5,5,9,8,17/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CALL SCOPNIT(LHEP)
C
      GO TO 100
C
   50 LIN=LCI
      WRITE(CMSSG,60)
   60 FORMAT('INPUT SWITCHED TO VDT ******')
      CALL MESSLOG(LOGUT,LOGUP)
C
  100 IF(LIN.EQ.LCI) WRITE(LOGUT,105)
  105 FORMAT(' SCOP->',$)
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
      IF(KMI.EQ.'    ') GO TO 130
      IF(KMI.EQ.'COM ') GO TO 130
      GO TO 140
C
  130 IF(LIN.NE.LCI) THEN
                     WRITE(CMSSG,110)IWDRAW
                     CALL MESSLOG(LOGUT,LOGUP)
                     ENDIF
      GO TO 100
C
  140 WRITE(CMSSG,110)IWDRAW
      IF(LIN.EQ.LCI) CALL MESSLOG(0,LOGUP)
      IF(LIN.NE.LCI) CALL MESSLOG(LOGUT,LOGUP)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMX.NE.'    ') GO TO 1000
C
      IDONE='    '
C
      CALL CALLER(IDONE,IERR)
C
      IF(IDONE.NE.'YES ') GO TO 150
C
      IF(IERR.EQ.0)       GO TO 100
C
      IF(LIN.NE.LCI)      GO TO 50
C
      GO TO 100
C
  150 WRITE(CMSSG,200)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      GO TO 1500
C
 1000 WRITE(CMSSG,1005)
 1005 FORMAT('Syntax Error - Command Ignored')
C
 1500 CALL MESSLOG(LOGUT,LOGUP)
      IF(LIN.NE.LCI) GO TO 50
      GO TO 100
      END
C$PROG CALLER    - Routine caller for SCOP
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 01/25/2000
C     ******************************************************************
C
      SUBROUTINE CALLER(IDONE,IERR)
C
      CHARACTER*4  IDONE
C
      IF(IDONE.NE.'YES ') CALL CMPSCOP(IDONE,IERR)
C
      RETURN
      END
