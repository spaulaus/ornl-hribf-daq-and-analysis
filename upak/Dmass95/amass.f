C$PROG AMASS
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/01/95
C     ******************************************************************
C
      IMPLICIT  NONE
C
      INTEGER*4 IA,IZ,ISOR,IERR,ERR(3),IV,KIND,MODE
C
      INTEGER*4 IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      REAL*8    EX,ER,AMU,AERR
C
      REAL*4    XV
C
      CHARACTER*4  KMD
C
      CHARACTER*12 CERR
C
      CHARACTER*1  CERR1(12)
C
      EQUIVALENCE (KMD,LWD(1,1)),(CERR,ERR),(CERR1,ERR)
C
      DATA         MODE/'MODE'/
C
C
  100 WRITE(6,105)
  105 FORMAT(' Enter nucleus->',$)
C
      READ(5,110)IWD
  110 FORMAT(20A4)
C
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,1,80,NTER)
C
      IF(KMD.EQ.'END ') CALL EXIT
      IF(KMD.EQ.'MODE') GO TO 200
                        GO TO 300
C
  200 CALL MILV(LWD(1,2),IV,XV,KIND,IERR)
      IF(IV.LT.1) IV=1
      IF(IV.GT.6) IV=6
      CALL MASSEX(MODE,IV,EX,ER,ISOR,IERR)
      GO TO 100
C
  300 CALL GETAZ(IWD,1,5,IA,IZ,IERR)
C
      IF(IERR.NE.0) THEN
                    WRITE(6,315)
                    GO TO 100
                    ENDIF
C
  315 FORMAT(1H ,'Syntax error - comd ignored')
C
      CALL MASSEX(IZ,IA,EX,ER,ISOR,IERR)
C
      IF(IERR.NE.0) THEN
                    WRITE(6,320) IERR
                    GO TO 100
                    ENDIF
C
  320 FORMAT(1H ,'MASSEX error no.',I3)
C
      AMU=DFLOAT(IA)+EX/931493.860D0
C
      AERR=ER/931493.860D0
C
      WRITE(CERR,325)AERR
  325 FORMAT(F12.9)
C
      CALL CONERR(CERR1)
C
      IF(ISOR.EQ.1) WRITE(6,330)AMU,ERR
      IF(ISOR.EQ.2) WRITE(6,340)AMU,ERR
      IF(ISOR.EQ.3) WRITE(6,350)AMU,ERR
      IF(ISOR.EQ.4) WRITE(6,360)AMU,ERR
C
  330 FORMAT(1H ,'AMU =',F14.9,2X,3A4,' from 95 Audi table')
  340 FORMAT(1H ,'AMU =',F14.9,2X,3A4,' from Garvey,s formula')
  350 FORMAT(1H ,'AMU =',F14.9,2X,3A4,' from Myers formula')
  360 FORMAT(1H ,'AMU =',F14.9,2X,3A4,' from Liquid drop formula')
C
      GO TO 100
C
      END
C$PROG CONERR
C
      SUBROUTINE CONERR(ERR)
C
      CHARACTER*1 ERR(12)
C
      DO 20 I=1,12
      IF(ERR(I).EQ.'0') GO TO 10
      IF(ERR(I).EQ.'.') GO TO 10
      IF(ERR(I).EQ.' ') GO TO 10
                        GO TO 30
   10 ERR(I)=' '
   20 CONTINUE
C
   30 CALL SQUEZL(ERR,1,12)
C
      RETURN
      END
