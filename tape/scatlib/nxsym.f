C$PROG NXSYM
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/24/2002
C     ******************************************************************
C
      SUBROUTINE NXSYM(ILO,IHI,KOP,SYMB,STAT)
C
      IMPLICIT INTEGER*4 (A-Z)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/SCATA/ MAXT,MAXS,SEC,ISET,NERR,MODEGO
      CHARACTER*4                 ISET,     MODEGO
      INTEGER*4     MAXT,MAXS,SEC,     NERR
C     ------------------------------------------------------------------
      COMMON/SCAT4/ LISI(400),SYM(3,50),OPR(50),NLS,NSY,DSPF
C     ------------------------------------------------------------------
C
      CHARACTER*4  SYMB(3)
C
      INTEGER*4    X20
C
      DATA         X20/'20'X/
C
      SAVE
C
C     ******************************************************************
C     PICKS UP ONE OPERATOR & SYMBOL AT A TIME
C     ******************************************************************
C
      STAT=0                            !SET GOOD STATUS
      KOP=1                             !SET DEFAULT OPERATOR +
      JT=1                              !SET DEFAULT OPERATOR +
      IF(ILO.GT.IHI) GO TO 100          !TST FOR OVERSHOOT
C
      DO 10 I=1,3                       !BLANK OUT THE SYMBOL
      SYMB(I)='    '
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
  200 WRITE(CMSSG,205)
  205 FORMAT('MORE THAN 12 CHARACTERS IN SCAT SYMBOL - NOT ALLOWED')
      CALL MESSLOG(LOGUT,LOGUP)
      WRITE(CMSSG,210)
  210 FORMAT('NO COMPUTED SCALERS PROVIDED')
      CALL MESSLOG(LOGUT,LOGUP)
      NERR=1
      RETURN
      END
