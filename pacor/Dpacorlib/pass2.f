C$PROG PASS2
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE PASS2
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/ML02/ IWDRAW(20)
C
      COMMON/III/  LIN,LCM,LCI
C
      COMMON/LLL/  MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
C
      CHARACTER*112 CMSSG
C
      EQUIVALENCE (CMSSG,MSSG)
C
      BYTE         DOLR,SEMI,EXCL,IBY(80)
      character*1  cDOLR, cSEMI, cEXCL
      equivalence  (cDOLR, DOLR), (cSEMI, SEMI), (cEXCL, EXCL)
C
      DATA         cDOLR, cSEMI, cEXCL/'$',';','!'/
C
      CHARACTER*4  KMD,IDONE,CIWD(20)
C
      EQUIVALENCE (KMD,LWD(1,1)),(IBY,IWD),(CIWD,IWD)
C
      SAVE
C
C     ************************************************************
C     READ SOURCE-FILE & PROCESS CONDITIONAL READOUT CODE
C     ************************************************************
C
      REWIND LIN
      NL=0
C
  100 READ(LIN,110,END=300)IWD
  110 FORMAT(20A4)
      NL=NL+1
C
      CALL LEGLASS(IWD,NL)
C
      IF(IBY(1).EQ.DOLR) GO TO 100
      IF(IBY(1).EQ.SEMI) GO TO 100
      IF(IBY(1).EQ.EXCL) GO TO 100
C
      CALL LISSOR('SOR ',IWD)
C
      NNON=0
      DO 115 I=1,20
      IWDRAW(I)=IWD(I)
      IF(CIWD(I).NE.'    ') NNON=NNON+1
  115 CONTINUE
      IF(NNON.LE.0) GO TO 100
C
      CALL CASEUP1(IWDRAW)
      CALL CASEUP(IWD)
C
      CALL GREAD(IWD,LWD,ITYP,NF,7,80,NTER)
C
      IDONE='    '
      IERR=0
C
      IF(KMD.EQ.'LOOP') THEN
                        CALL LOOPEX(LIN,LCI)
                        GO TO 100
                        ENDIF
C
      CALL CALLER(IDONE,IERR)
C
      IF(IERR.NE.0)       GO TO 100
      IF(IDONE.EQ.'YES ') GO TO 100
C   
      WRITE(CMSSG,200)
      CALL ERRLOG(LOGUT,LOGUP)
  200 FORMAT('COMMAND NOT RECOGNIZED ******')
      GO TO 100
C
  300 CALL CONDICO
      RETURN
      END
