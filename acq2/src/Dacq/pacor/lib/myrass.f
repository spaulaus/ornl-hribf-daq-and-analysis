C$PROG MYRASS   - Processes MyRIAD assignment statement
C
C     ************************************************************
C     Robert Varner ORNL Physics - Created 01 August 2015
C     ************************************************************
C
      SUBROUTINE MYRASS(IWD)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC7/ISORL,IEXPL,LISTYP,NERR
C
      COMMON/MYR/ MYRID(3)
C     ------------------------------------------------------------------
      INTEGER*4    IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      INTEGER*4    X8000
      DATA         X8000/'8000'X/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL GREAD(IWD,LWD,ITYP,NF,5,80,NTER)
C
      IF(NTER.NE.0) GO TO 500
C
      CALL MILV(LWD(1,1),ITST,XV,NF,KIND,IERR)
C
      IF(IERR.NE.0)     GO TO 500
      IF(ITST.LE.0)     GO TO 500
      IF(ITST.GT.32767) GO TO 500
C
      CALL MILV(LWD(1,2),JTST,XV,NF,KIND,IERR)
C
      IF(IERR.NE.0)     GO TO 500
      IF(JTST.LE.0)     GO TO 500
      IF(JTST.GT.32767) GO TO 500
C
      IF(ITST.EQ.JTST)  GO TO 510
C
      CALL MILV(LWD(1,3),KTST,XV,NF,KIND,IERR)
C
      IF(IERR.NE.0)     GO TO 500
      IF(KTST.LE.0)     GO TO 500
      IF(KTST.GT.32767) GO TO 500
C
      IF ((JTST .EQ. KTST) .OR. (ITST .EQ. KTST))  GO TO 510
C
      MYRID(1)=ITST+X8000
      MYRID(2)=JTST+X8000
      MYRID(3)=KTST+X8000
C
      RETURN
C
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error or illegal value in MYRIAD ID specification')
      GO TO 600
C
  510 WRITE(CMSSG,515)ITST,JTST
  515 FORMAT('Specified MYRIAD IDs',2I8,' are the same - not allowed')
      GO TO 600
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      RETURN
      END
C
