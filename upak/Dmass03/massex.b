
C
      DATA MODE/6/
C
C     **************************************************************
C     ROUTINE TO COMPUTE MASS EXCESS (KEV)
C     **************************************************************
C     MODE=1 SAYS USE MASS TABLE ONLY
C     MODE=2 SAYS USE GARVY'S FORMULAS ONLY
C     MODE=3 SAYS USE MYERS'  FORMULAS ONLY
C     MODE=4 SAYS USE MASS TABLE OR GARVY
C     MODE=5 SAYS USE MASS TABLE OR MYERS
C     MODE=6 SAYS USE MASS TABLE OR GARVY OR MYERS
C     **************************************************************
C     ABUF(1,J) - CONTAINS MASS NO.          FOR JTH ENTRY
C     ABUF(2,J) - CONTAINS MASS EXCESS (KEV) FOR JTH ENTRY
C     ABUF(3,J) - CONTAINS UNCERTAINTY (KEV) FOR JTH ENTRY
C     **************************************************************
C
      IERR=0                            !RESET ERROR FLAG
      IF(IZ.NE.'MODE') GO TO 10         !TST FOR MODE SET REQUEST
      IF(IA.GE.1.AND.IA.LE.6) GO TO 5   !TST FOR LEGAL
      IERR=2
      RETURN
C
    5 MODE=IA                           !SET THE MODE REQUESTED
      RETURN
C
C     **************************************************************
C     THIS A NORMAL MASS-EXCESS REQUEST
C     **************************************************************
C
   10 IF(MODE.EQ.1) GO TO 15            !TST MODE OF OPERATION
      IF(MODE.EQ.2) GO TO 50
      IF(MODE.GE.4) GO TO 15
      GO TO 100
C
C     **************************************************************
C     LOOK FOR IT IN MASS TABLE
C     **************************************************************
C
   15 NDX=IZ+1                          !INDEX IN LZLO-LZHI PNTR LIST
      IF(NDX.GT.119) GO TO 100          !TST FOR OUT OF RANGE
C
      IAA=LZLO(NDX)                     !COMPUTE THE ABUF LO-LIMIT
      IBA=LZHI(NDX)                     !COMPUTE THE ABUF HI-LIMIT
C
      DO 20 J=IAA,IBA                   !LOOK FOR REQUIRED A-NUMBER
      IAT=ABUF(1,J)+0.5                 !CONV TO INTEGER
      IF(IAT.EQ.IA) GO TO 30            !TST AGAINST REQUEST
   20 CONTINUE
      GO TO 40                          !NOT FOUND, USE OTHER METHOD
C
   30 EX=ABUF(2,J)                      !SET MASS EXCESS
      ER=ABUF(3,J)                      !SET ASSOCIATED UNCERTAINTY
      ISOR=1                            !SET "SOURCE" FLAG
      RETURN
C
C     **************************************************************
C     CALCULATE THE MASS EXCESS FROM GARVY'S FORMULAS
C     **************************************************************
C
   40 IF(MODE.EQ.4) GO TO 50
      IF(MODE.EQ.6) GO TO 50
      GO TO 100
C
   50 IN=IA-IZ
      IF(IN.LT.10.OR.IN.GT.154) GO TO 100
      IF(IZ.LT. 6.OR.IZ.GT.100) GO TO 100
      IF(IA.LT.16.OR.IA.GT.253) GO TO 100
      IF(IN.LT.IZ)              GO TO 100
C
      BMEV=110.7824+GN(IN)+GZ(IZ)+GA(IA)
      PEX=8.0714*IN+7.289*IZ
      EXMEV=PEX-BMEV
      EX=1000.0*EXMEV
      ER=2000.0
      ISOR=2
      RETURN
C
C     **************************************************************
C     CALCULATE THE MASS EXCESS USING MYERS'S "MASS FORMULA"
C     **************************************************************
C
  100 IF(MODE.EQ.3) GO TO 110
      IF(MODE.GE.5) GO TO 110
      IERR=1
      RETURN
C
  110 EX=1000.0*QMQMASX(IZ,IA,ISW,IERR)
      ER=5000.0
      ISOR=3
      IF(ISW.EQ.1) ISOR=4
      RETURN
      END
