C$PROG ITERV
      FUNCTION ITERV(IWD,IA,IB,IERR,MSER)
C
      INTEGER*4 IWD(1)
      INTEGER*4 MSG(10,1),MSER(10)
      CHARACTER*40 MSC(1)
      INTEGER*4 LWD(2,40),ITYP(40)
      INTEGER*4 NAMS(2)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'SYNTAX ERROR IN TERM DEFINITION         '/
C
      SAVE
C
C
C     **************************************************************
C     EVALUATES TERMS OF THE FORM - #         (ASCII NUMBER)
C                                 - SYMB      (SIMPLE SYMBOL)
C                                 - SYMB(#)   (SUBSCRIPTED SYMBOL)
C                                 - SYMB(SYM) (SUBSCRIPTED SYMBOL)
C                                 - (SYM MUST BE NON-SUBSCRIPTED VAR)
C     **************************************************************
C
      IERR=0
      LLP=IFIND(IWD,z'28',IA,IB)              !FIND "(" IF ANY
      LRP=IFIND(IWD,z'29',IA,IB)              !FIND ")" IF ANY
      IF(LLP.EQ.0.AND.LRP.EQ.0) GO TO 50      !IF NONE, DO SIMPLE VALUE
      IF(LLP.EQ.0.OR.LRP.EQ.0)  GO TO 210     !TST FOR ERROR
      IF(LLP.GE.LRP)            GO TO 210     !TST FOR ERROR
C
C     **************************************************************
C     EVALUATE THE FORM - SYMBOL(#)  OR  SYMBOL(SYMB)
C     **************************************************************
C
      CALL REFOR(IWD,LWD,ITYP,NF,IA,LLP,NTER) !RE-FORMAT NAME PART
C
      IF(NTER.NE.0) GO TO 210                 !TST FOR ERROR
      IF(NF.NE.1)   GO TO 210                 !TST FOR ERROR
      IF(ITYP(1).NE.1) GO TO 210              !REQUIRE ALPHA TYPE
      NAMS(1)=LWD(1,1)                        !SAVE SYMBOL
      NAMS(2)=LWD(2,1)                        !SAVE SYMBOL
C
      CALL REFOR(IWD,LWD,ITYP,NF,LLP,IB,NTER) !RE-FORMAT "INDEX"
C
      IF(NTER.NE.0) GO TO 210                 !TST FOR ERROR
      IF(NF.NE.1)   GO TO 210                 !REQUIRE 1 FIELD ONLY
      NDX=ISVAV(LWD,IERR,MSER)                !GET INDEX VALUE
      IF(IERR.NE.0) GO TO 320                 !TST FOR ERROR
      ITERV=IDVAV(NAMS,NDX,IERR,MSER)         !VALUE OF INDEXED VAR
      IF(IERR.NE.0) GO TO 320                 !TST FOR ERROR
      RETURN
C
C     **************************************************************
C     EVALUATE SIMPLE VARIABLE OR NUMBER
C     **************************************************************
C
   50 CALL REFOR(IWD,LWD,ITYP,NF,IA,IB,NTER)  !RE-FORMAT
      IF(NTER.NE.0) GO TO 210                 !TST FOR ERROR
      IF(NF.NE.1)   GO TO 210                 !REQUIRE 1 FIELD ONLY
      ITERV=ISVAV(LWD,IERR,MSER)              !GET SIMPLE VALUE
      IF(IERR.NE.0) GO TO 320                 !TST FOR ERROR
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP ERROR MESSAGE
C     **************************************************************
C
  210 JJ=1
      GO TO 300
C
  300 DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      IERR=JJ
  320 ITERV=0                                 !RETURN ZERO VALUE
      RETURN
      END
