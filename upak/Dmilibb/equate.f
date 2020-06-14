C$PROG EQUATE    - Processes equate statements for symbol definitions
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      SUBROUTINE EQUATE(IDONE,IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C   
      COMMON/ML03/ ISYN(500),ISYV(500),NSYM
C
      CHARACTER*8  ISYN
C
      CHARACTER*4  IDONE
C   
      INTEGER*4    KSYM(2)
      CHARACTER*8  CSYM
      EQUIVALENCE (CSYM,KSYM)
C   
      DATA IEQL,ISEM/Z'3D',Z'3B'/
C
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
      IDONE='    '
C   
      LEQ=IFIND(IWD,IEQL,1,80)
      IF(LEQ.LE.0) RETURN
C   
      IA=NXNB(IWD,LEQ+1,80)
      IF(IA.LE.0)  GO TO 100
      IB=NXBL(IWD,IA,80)
      IF(IA.LE.0)  GO TO 100
      IB=IB-1
      IV=KVALU(IWD,IA,IB,IERR)
      IF(IERR.NE.0)GO TO 100
C   
      KSYM(1)=LWD(1,1)
      KSYM(2)=LWD(2,1)
C
      CALL SYMSAV(CSYM,IV,IERR)
C   
      IF(IERR.EQ.0) THEN
                    WRITE(CMSSG,10)CSYM,IV
                    CALL SQUEZL(MSSG,1,24)
                    CALL MESSLOG(LOGUT,LOGUP)
                    ENDIF
C   
      IF(IERR.NE.0) THEN
                    WRITE(CMSSG,20)CSYM
                    CALL MESSLOG(LOGUT,LOGUP)
                    ENDIF
C   
   10 FORMAT(A,' = ',I10)
   20 FORMAT(A,' - UNDEFINED')
C   
      IDONE='YES '
      RETURN
C   
  100 WRITE(CMSSG,110)
  110 FORMAT('SYNTAX ERROR IN SYMBOL DEFINITION')
      CALL MESSLOG(LOGUT,LOGUP)
      IDONE='YES '
      IERR=1
      RETURN
      END
