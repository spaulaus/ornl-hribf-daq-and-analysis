C$PROG CLOCASS   - Processes Clock assignment statement
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 01/06/2004
C     ************************************************************
C
      SUBROUTINE CLOCASS(IWD)
C
C     ------------------------------------------------------------------
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4    MSSG,NAMPROG,LOGUT,LOGUP,LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/PAC7/ISORL,IEXPL,LISTYP,NERR
C
      COMMON/KLOC/ KLOCID(2)
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
      KLOCID(1)=ITST+X8000
      KLOCID(2)=JTST+X8000
C
      RETURN
C
C     ------------------------------------------------------------------
C     Error return
C     ------------------------------------------------------------------
C
  500 WRITE(CMSSG,505)
  505 FORMAT('Syntax error or illegal value in clock-ID specification')
      GO TO 600
C
  510 WRITE(CMSSG,515)ITST,JTST
  515 FORMAT('Specified clock IDs',2I8,' are the same - not allowed')
      GO TO 600
C
  600 CALL MESSLOG(LOGUT,LOGUP)
      NERR=NERR+1
      RETURN
      END
C
