C$PROG GSPAN
      SUBROUTINE GSPAN(IWD,IA,IB,IRP,NF,LVAL,NAME,IERR,MSER)
C
      INTEGER*4 IWD(1),LIMF(2,100),LVAL(100)
C
      INTEGER*4 LABL(7),MSG(10,3),MSER(10)
      CHARACTER*40 MSC(3)
      CHARACTER*28 LABLC
C
      EQUIVALENCE (MSC(1),MSG(1,1)),(LABLC,LABL(1))
C
      DATA MSC/
     1'ILLEGAL LIST LABEL OR SYNTAX ERROR      ',
     2'LEFT PARENTHESIS NOT FOUND              ',
     3'MATCHING RIGHT PARENTHESIS NOT FOUND    '/
C
      DATA LABLC/'H   OH  L   R   G   GS  B   '/
C
      INTEGER*4   BLANK
      character*4 cblank
      equivalence (cblank,blank)
      DATA       cBLANK/'    '/
C
      INTEGER*4   X20,X28,X29,X2C
      DATA        X20,X28,X29,X2C/z'20',z'28',z'29',z'2C'/
C
      CHARACTER*4 LASTC
C
      SAVE
C
C     **************************************************************
C     FINDS NEXT REGION IN IWD (IN THE RANGE IA THRU IB)
C
C     REGION IS OF THE FORM - LABEL(CONTENTS.....)
C
C     ON RETURN - NAME CONTAINS LABEL
C               - IRP  CONTAINS LOCATION OF ")" DEFINING CONTENTS
C     **************************************************************
C
      IERR=-1                               !SET ERROR CODE TO EMPTY
      NNB=0                                 !ZERO NON-BLANK COUNTER
      N=0                                   !ZERO BYTE COUNTER
      NAME=BLANK                            !SET NAME TO BLANK
C
      DO 20 I=IA,IB                         !LOAD NAME & FIND "("
      CALL ILBYTE(IT,IWD,I-1)
      IF(IT.EQ.X20) GO TO 20                !IGNORE BLANKS
      NNB=NNB+1                             !INC NON-BLANK COUNTER
      IF(IT.EQ.X28) GO TO 40                !TST FOR "("
      N=N+1                                 !INC NAME-BYTE CNTR
      IF(N.GT.3) GO TO 210                  !TST FOR TOO MANY
      CALL ISBYTE(IT,NAME,N-1)              !LOAD BYTE INTO NAME
   20 CONTINUE
      IF(NNB.EQ.0) RETURN                   !EVERYTHING IS EMPTY
      GO TO 220                             !ERROR IF "(" NOT FOUND
C
   40 NLP=1                                 !LEFT  PERIN COUNTER
      NRP=0                                 !RIGHT PERIN COUNTER
      IS=I+1                                !INC SCAN PNTR
      IERR=0                                !RESET ERROR CODE
      LNON=0                                !LOC OF NON-DELIMITER
      LDEL=0                                !LOC OF     DELIMITER
      NF=0                                  !# OF FIELDS
      LASTC='NON '                          !LAST CHAR = NON-DEL
C
      DO 60 I=IS,IB                         !FIND MATCHING ")"
      CALL ILBYTE(IT,IWD,I-1)               !GET NEXT BYTE
      IF(IT.EQ.X28) NLP=NLP+1               !TST & INC (-CNTR
      IF(IT.EQ.X29) NRP=NRP+1               !TST & INC )-CNTR
      IF(NRP.GE.NLP)  GO TO 100             !TST FOR ( ) MATCH
      IF(IT.EQ.X20) GO TO 50                !TST FOR BLANK
      IF(IT.EQ.X2C) GO TO 50                !TST FOR COMMA
C
      IF(LNON.EQ.0) LNON=I                  !RESET NON-DEL LOC
      LASTC='NON '                          !LAST CHAR = NON-DEL
      GO TO 60
C
   50 IF(LASTC.EQ.'DEL ') GO TO 60          !TST FOR LAST CHAR = DEL
      IF(LNON.EQ.0)       GO TO 60          !TST FOR NON-DEL FOUND
      NF=NF+1                               !IF NOT, INC # FIELDS
      LIMF(1,NF)=LNON                       !SAVE FIELD LO-LIMIT
      LIMF(2,NF)=I-1                        !SAVE FIELD HI-LIMIT
      LNON=0                                !RESET NON-DEL LOC
      LASTC='DEL '                          !LAST CHAR = DEL
   60 CONTINUE
      GO TO 230                             !ERROR IF MATCH NOT FOUND
C
  100 IRP=I                                 !SAVE LOCATION OF )
      IF(LNON.EQ.0) GO TO 105               !TST FOR ANOTHER FIELD
      NF=NF+1                               !INC FOR LAST FIELD
      LIMF(1,NF)=LNON                       !SAVE FIELD LO-LIMIT
      LIMF(2,NF)=I-1                        !SAVE FIELD HI-LIMIT
  105 DO 110 I=1,7                          !TST NAME FOR LEGAL
      IF(NAME.EQ.LABL(I)) GO TO 120
  110 CONTINUE
      GO TO 210                             !ERROR IF NOT IN LIST
C
C     **************************************************************
C     GENERATE ASSOCIATED VALUE LIST - LVAL
C     **************************************************************
C
  120 DO 130 J=1,NF
      LVAL(J)=ITERV(IWD,LIMF(1,J),LIMF(2,J),IERR,MSER)
      IF(IERR.NE.0) RETURN
  130 CONTINUE
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP MESSAGE
C     **************************************************************
C
  210 JJ=1
      GO TO 300
  220 JJ=2
      GO TO 300
  230 JJ=3
      GO TO 300
C
  300 DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      IERR=JJ
      RETURN
      END
