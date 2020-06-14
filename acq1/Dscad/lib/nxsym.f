C$PROG NXSYM     - Picks up one operator & symbol at a time for SCAD
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 08/08/2004
C     ******************************************************************
C
      SUBROUTINE NXSYM(ILO,IHI,KOP,SYMB,STAT)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/SD05/ LISI(400),SYM(3,50),OPR(50),NLS, NSY, DSPF
      INTEGER*4    LISI,     SYM,      OPR,    NLS, NSY, DSPF
C     ------------------------------------------------------------------
      COMMON/SD06/ NERR
      INTEGER*4    NERR
C     ------------------------------------------------------------------
      INTEGER*4    SYMB(3)
C
      INTEGER*4    X20
      DATA         X20/'20'X/
C
      SAVE
C
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     PICKS UP ONE OPERATOR & SYMBOL AT A TIME
C     ------------------------------------------------------------------
C
      STAT=0                            !SET GOOD STATUS
      KOP=1                             !SET DEFAULT OPERATOR +
      JT=1                              !SET DEFAULT OPERATOR +
      IF(ILO.GT.IHI) GO TO 100          !TST FOR OVERSHOOT
C
      DO 10 I=1,3                       !BLANK OUT THE SYMBOL
      SYMB(I)='20202020'X
   10 CONTINUE
C
      IA=ILO                            !FIRST BYTE TO EXAM
      DO 20 I=IA,IHI                    !LOOP ON REMAINING BYTES
      CALL ILBYTE(IT,LISI,I-1)          !PICK UP BYTE FROM LISI
      IF(IT.EQ.X20) GO TO 20            !IGNORE IF BLANK
      JT=KINOP(IT)                      !GET OPERATOR TYPE
      IF(JT.NE.0)   GO TO 30            !OPERATOR IF NON-ZERO
      GO TO 40                          !FIRST BYTE OF SYMBOL
   20 CONTINUE
C
   30 KOP=JT                            !SAVE OPERATOR TYPE
      I=I+1                             !INC SCAN PNTR
C
   40 IS=I                              !FIRST BYTE # FOR SYMBOL SCAN
      IA=NXNB(LISI,IS,IHI)              !FIND NEXT NON-BLANK
      IF(IA.LE.0) GO TO 100             !ERROR IF NOT FOUND
C
      DO 50 I=IA,IHI                    !LOOP ON REMAINING BYTES
      CALL ILBYTE(IT,LISI,I-1)          !PICK UP BYTE
      IF(IT.EQ.X20) GO TO 70            !DONE THIS SCAN IF BLANK
      JT=KINOP(IT)                      !GET OPERATOR TYPE
      IF(JT.NE.0)   GO TO 70            !DONE THIS SCAN IF OPERATOR
   50 CONTINUE
      I=IHI+1                           !FRIG INDEX
C
   70 IB=I-1                            !MAX BYTE # TO LOAD IN SYMB
C
      IF(IB-IA.GT.11) GO TO 200
C
      CALL LODUP(LISI,IA,IB,SYMB,1)     !LOAD UP SYMBOL
C
      ILO=IB+1                          !LO-BYTE FOR NEXT CALL
      RETURN
C
  100 STAT=1                            !ERROR RETURN
      RETURN
C
  200 WRITE(6,205)
  205 FORMAT(1H ,'MORE THAN 12 CHARACTERS IN SYMBOL - NOT ALLOWED')
      WRITE(6,210)
  210 FORMAT(1H ,'NO COMPUTED SCALERS PROVIDED')
      NERR=1
      RETURN
      END
