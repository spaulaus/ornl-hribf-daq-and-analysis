C$PROG ISVAL     - Returns a symbol value
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/19/2002 - for gnu
C     ******************************************************************
C
      FUNCTION ISVAL(IWD,ITYP,IERR)
C   
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
C   
      COMMON/ML03/ ISYN(500),ISYV(500),NSYM
      CHARACTER*8  ISYN
C   
      INTEGER*4    IWD(2),JWD(2)
      CHARACTER*8  SYM 
      EQUIVALENCE (SYM,JWD)
C
      SAVE
C   
      IERR=0
C   
      IF(ITYP.EQ.1) GO TO 100
C   
      CALL MILV(IWD,IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
      IF(KIND.NE.1) GO TO 200
      ISVAL=IV
      RETURN
C   
  100 JWD(1)=IWD(1)
      JWD(2)=IWD(2)
C
      DO 110 I=1,NSYM
      IF(SYM.EQ.ISYN(I)) GO TO 120
  110 CONTINUE
      GO TO 200
C   
  120 ISVAL=ISYV(I)
      RETURN
C   
  200 WRITE(CMSSG,210)IWD
  210 FORMAT('ERROR EVALUATING SYMBOL OR CONSTANT = ',2A4)
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      ISVAL=0
      RETURN
      END
