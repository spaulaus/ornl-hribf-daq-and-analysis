C$PROG KVALU
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 04/30/90
C     ************************************************************
C
      FUNCTION KVALU(IWD,KA,KB,IERR)
C
      INTEGER*4 IWD(*)
C
      DATA ICOM,ILSQ,IRSQ/'2C'X,'5B'X,'5D'X/
C
      CHARACTER*4  MODT
C
      SAVE
C
      IA=KA
      IERR=0
      IVAL=0
      IX=IA+3                           !CHECK FOR "MOD" FUNCTION
      CALL LODUP(IWD,IA,IX,MODT,1)
      IF(MODT.NE.'MOD(') GO TO 100      !IF NO, GO EVALUATE NORMALLY
      IA=IA+4                           !SKIP TO ARG1
      IB=IFIND(IWD,ICOM,IA,KB)-1        !LOOK FOR COMMA
      IF(IB.LE.0) GO TO 301             !TST FOR ERROR
      IV1=KVALUE(IWD,IA,IB,IERR)        !EVALUATE ARG1
      IF(IERR.NE.0) GO TO 310           !TST FOR ERROR
      IA=IB+2                           !SKIP TO ARG2
      IB=IFIND(IWD,'29'X,IA,KB)-1       !LOOK FOR ")"
      IF(IB.LT.0) GO TO 301             !TST FOR ERROR
      IV2=KVALUE(IWD,IA,IB,IERR)        !EVALUATE ARG2
      IF(IERR.NE.0) GO TO 310           !TST FOR ERROR
      IVAL=MOD(IV1,IV2)                 !COMPUTE "REMAINDER"
      GO TO 200                         !AND HEAD FOR HOME
C
  100 IF(IA.GT.KB) GO TO 200            !TST FOR DONE
      IX=IFIND(IWD,ILSQ,IA,KB)          !LOOK FOR [ - BIT LIST
      IF(IX.GT.0) GO TO 110             !IF YES, TST FOR PRECEEDING FIEL
      IVAL=IVAL+KVALUE(IWD,IA,KB,IERR)  !OTHERWISE, EVAL EXPRESSION
      IF(IERR.NE.0) GO TO 310           !TST FOR ERROR
      GO TO 200                         !OR GO HOME
C
  110 LLSQ=IX                           !LOCATION OF [
      IF(IX.EQ.IA) GO TO 120            !NO PRECEEDING FIELD
      IB=LLSQ-1                         !HI-BYTE OF FIELD
      IVAL=IVAL+KVALUE(IWD,IA,IB,IERR)  !EVALUATE EXPRESSION
      IF(IERR.NE.0) GO TO 310
C
  120 IBIV=0                            !ZERO BIT ACCUMULATOR
      IA=LLSQ+1                         !LO-BYTE OF BIT-LIST
  130 IF(IA.GT.KB) GO TO 160            !TST FOR DONE
      IX=IFIND(IWD,IRSQ,IA,KB)          !LOOK FOR ] - END OF LIST
      IF(IX.LE.0) GO TO 301             !TST FOR ERROR
      LRSQ=IX                           !LOCATION OF ]
      IHI=LRSQ-1                        !HI-BYTE OF BIT-LIST
      IX=IFIND(IWD,ICOM,IA,IHI)         !LOOK FOR "," - END OF FIELD
      LCOM=IX                           !LOCATION OF COMMA
      IF(IX.LE.0) IX=IHI+1
      IB=IX-1                           !HI-BYTE OF FIELD
      IBIT=KVALUE(IWD,IA,IB,IERR)       !BIT # (LO-BIT = 1 BASIS)
      IF(IERR.NE.0) GO TO 310           !TST FOR ERROR
      IF(IBIT.GT.24) GO TO 302          !TST FOR TOO BIG
      IF(IBIT.LT.1)  GO TO 302          !TST FOR TOO SMALL
      IBN=IBIT-1
      IBIV=IBSET(IBIV,IBN)              !SET THE MIGHTY BIT
      IF(LCOM.EQ.0) GO TO 140           !TST FOR LAST FIELD
      IA=LCOM+1                         !OTHERWISE, BUMP SCAN PNTR
      GO TO 130                         !AND GO FOR NEXT FIELD
C
  140 IA=LRSQ+1                         !BUMP SCAN PNTR
      IVAL=IVAL+IBIV                    !ADD BIT-TERM
      GO TO 100                         !AND GO BACK TO "TOP"
C
  160 IVAL=IVAL+IBIV                    !BIT-LIST ENDS FULL RANGE
C
  200 KVALU=IVAL                        !SET THE FUNCTION
      RETURN                            !AND GO HOME
C
  301 IERR=1                            !SYNTAX ERROR
      GO TO 310
  302 IERR=17                           !ILLEGAL BIT VALUE
  310 KVALU=0
      RETURN
      END
