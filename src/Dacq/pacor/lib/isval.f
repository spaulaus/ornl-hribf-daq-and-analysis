C$PROG ISVAL
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      FUNCTION ISVAL(IWD,JTYP,IERR)
C   
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C   
      COMMON/ML03/ ISYN(100),ISYV(100),NSYM
C   
      INTEGER*4 IWD(2)
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      SAVE
C
C     ************************************************************
C     RETURNS VALUE ASSOCIATED WITH SYMBOL OR ASCII CONSTANT 
C     CONTAINED IN IWD
C     ************************************************************
C   
      IERR=0
      ITYP=JTYP
      IF(ITYP.EQ.0) ITYP=ITYPER(IWD)
C
      IF(ITYP.EQ.0) GO TO 500   
      IF(ITYP.EQ.1) GO TO 100
      IF(ITYP.EQ.2) GO TO 200
      IF(ITYP.EQ.3) GO TO 300
C
  100 DO 110 I=1,NSYM
      IF(IWD(1).EQ.ISYN(I)) THEN
                            ISVAL=ISYV(I)
                            RETURN
                            ENDIF
  110 CONTINUE
      GO TO 500
C   
  200 CALL MILV(IWD,IV,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 500
      IF(KIND.NE.1) GO TO 500
      ISVAL=IV
      RETURN
C
  300 CALL HEXVAL(IWD,IV,IERR)
      IF(IERR.NE.0) GO TO 500
      ISVAL=IV
      RETURN
C
  500 WRITE(CMSSG,510)IWD
  510 FORMAT('ERROR EVALUATING SYMBOL OR CONSTANT = ',2A4)
      CALL ERRLOG(LOGUT,LOGUP)
      IERR=1
      ISVAL=0
      RETURN
      END
