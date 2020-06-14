C$PROG NUMASS
C
      INTEGER*4 IWD(20),LWD(2,40),ITYP(40),JWD(2)
C
      REAL*8    EX,ER,AMU,AER
C
      CHARACTER*40 SORC(4)
C
      CHARACTER*4  IWD1,KMD
C
      EQUIVALENCE (IWD1,IWD(1))
C
      EQUIVALENCE (JWD,IWD),(KMD,LWD(1,1))
C
      INTEGER*4    IMODE
      DATA         IMODE/'MODE'/
C
      DATA SORC/"  From Audi & Wapstra's 1995 mass table ",
     &          "  From Garvy's mass formula             ",
     &          "  From Myers' formula + shell correction",
     &          "  From Myers' formula - liquid drop only"/
C
  100 WRITE(6,105)
  105 FORMAT(' Enter nuclear name ->',$)
C
      READ(5,110)IWD
  110 FORMAT(20A4)
C
      CALL CASEUP(IWD)
C
      IF(IWD1.EQ.'END ') STOP
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(NTER.NE.0) GO TO 200
C
      IF(KMD.EQ.'MODE') THEN
      CALL MILV(LWD(1,2),MODE,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 200
      IF(MODE.LE.0) MODE=6
      IF(MODE.GT.3) MODE=6
      CALL MASSEX(IMODE,MODE,EX,ER,ISOR,IERR)
      GO TO 100
      ENDIF
C
      CALL GETAZ(IWD,1,8,IA,IZ,IERR)
C
      IF(IERR.NE.0) GO TO 200
C
      CALL MASSEX(IZ,IA,EX,ER,ISOR,IERR)
C
C
      AMU=DFLOAT(IA)+EX/931494.32D0
C
      AER=           ER/931494.32D0
C
      WRITE(6,120)JWD,AMU,SORC(ISOR)
      WRITE(6,125)AER
C
  120 FORMAT(1H ,A4,A1,F15.9,A)
  125 FORMAT(1H ,5X,      F15.9)
C
      GO TO 100
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'Syntax error or illegal value - command ignored')
      GO TO 100
C
      END
