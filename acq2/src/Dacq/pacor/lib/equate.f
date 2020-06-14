C$PROG EQUATE
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE EQUATE(IDONE,IERR)
C   
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      INTEGER*4     JWD(20)
      CHARACTER*4  CJWD(20)
      EQUIVALENCE (CJWD,JWD)
C   
      DATA IEQL,ISEM/'3D'X,'3B'X/
C
      CHARACTER*4  IDONE
C
      SAVE
C   
C     ************************************************************
C     PROCESS - SYM=EXPRESSION (EVALUATES EXPRESSION & SAVES SYM)
C     ************************************************************
C
      IERR=0
      IDONE='    '
C   
      DO 10 I=1,20
      JWD(I)=IWD(I)
   10 CONTINUE
C
      LEQ=IFIND(JWD,IEQL,7,80)
      IF(LEQ.LE.0) RETURN
C
      IF(CJWD(1).NE.'    ') GO TO 110
C   
      IA=NXNB(JWD,LEQ+1,80)
      IF(IA.LE.0)  GO TO 100
      CALL SQUEZL(JWD,IA,80)
      IB=LSNB(JWD,IA,80)
      IF(IB.LE.0)  GO TO 100
      IV=KVALU(JWD,IA,IB,IERR)
      IF(IERR.NE.0)GO TO 100
C   
      KSYM=LWD(1,1)
      CALL SYMSAV(KSYM,IV,IERR)
      IDONE='YES '
      RETURN
C   
  100 WRITE(CMSSG,105)
  105 FORMAT('SYNTAX ERROR IN SYMBOL DEFINITION')
      GO TO 200
  110 WRITE(CMSSG,115)
  115 FORMAT('LABEL NOT ALLOWED ON SYMBOL DEFINITION STATEMENT')
C
  200 CALL ERRLOG(LOGUT,LOGUP)
      IDONE='YES '
      IERR=1
      RETURN
      END
