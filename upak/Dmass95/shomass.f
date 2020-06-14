C$PROG SHOMASS
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/01/95
C     ******************************************************************
C
      IMPLICIT  NONE
C
      INTEGER*4 IA,IZ,IERR,ISOR
C
      INTEGER*4 IWD(20)
C
      REAL*8    EX,ER,AMU,AERR
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
      IF(IWD(1).EQ.'END ') CALL EXIT
C
      CALL GETAZ(IWD,1,5,IA,IZ,IERR)
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
      WRITE(6,325)AMU,AERR
  325 FORMAT(1H ,'AMU =',F14.9,' +- ',F14.9)
C
      GO TO 100
C
      END
