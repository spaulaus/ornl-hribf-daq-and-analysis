C$PROG EQUATE
      SUBROUTINE EQUATE(IWD,IA,IB,IEQ,IERR,MSER)
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
C
      INTEGER*4 IWD(1)
      INTEGER*4 LWD(2,40),ITYP(40)
      INTEGER*4 MSG(10,8),MSER(10),NAMS(2)
      CHARACTER*40 MSC(8)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/
     1'SYNTAX ERROR - LEFT OF "= SIGN"         ',
     2'SYMBOL NOT FOUND IN TABLE ----- XXXXXXXX',
     3'ILLEGAL INDEXING OF ----------- XXXXXXXX',
     4'ILLEGAL INDEX VALUE ----------- XXXXXXXX',
     5'INDEX REQUIRED FOR ------------ XXXXXXXX',
     6'RIGHT SIDE OF "= SIGN" IS NULL          ',
     7'SYMBOL TABLE OVERFLOW (MAX # = 100)     ',
     8'# OF SYMBOL VALUES EXCEEDS 16384        '/
C
      DATA MAXNS,MAXNV/100,16384/
C
      SAVE
C
C     **************************************************************
C     PROCESS EQUATES - SYMB(SYM) = EXPRESSION
C                     - SYMB(#)   = EXPRESSION
C                     - SYM       = EXPRESSION
C     (HERE SYM DENOTES NON-SUBSCRIPTED VARIABLE)
C     **************************************************************
C
      IERR=0
      LLP=IFIND(IWD,Z'28',IA,IEQ)             !FIND "(" IF ANY
      LRP=IFIND(IWD,Z'29',IA,IEQ)             !FIND ")" IF ANY
      IF(LLP.EQ.0.AND.LRP.EQ.0) GO TO 50      !IF NONE, SIMPLE VAR
      IF(LLP.EQ.0.OR.LRP.EQ.0)  GO TO 210     !TST FOR ERROR
      IF(LLP.GE.LRP)            GO TO 210     !TST FOR ERROR
C
C     **************************************************************
C     LEFT-HAND SIDE IS FORM - SYMB(#)  OR  SYMB(SYM)
C     **************************************************************
C
      CALL REFOR(IWD,LWD,ITYP,NF,IA,LLP,NTER) !RE-FORMAT NAME PART
C
      IF(NTER.NE.0) GO TO 210                 !TST FOR ERROR
      IF(NF.NE.1)   GO TO 210                 !TST FOR ERROR
      IF(ITYP(1).NE.1) GO TO 210              !REQUIRE ALPHA TYPE
      LDX=LOCSYM(LWD(1,1))                    !FIND IT IN TABLE
      IF(LDX.LE.0) GO TO 220                  !TST FOR EXIST
      IF(ISYT(LDX).NE.2) GO TO 230            !REQUIRE INDEXED VAR
C
      CALL REFOR(IWD,LWD,ITYP,NF,LLP,IEQ,NTER)!RE-FORMAT "INDEX"
C
      IF(NTER.NE.0) GO TO 210                 !TST FOR ERROR
      IF(NF.NE.1)   GO TO 210                 !REQUIRE 1 FIELD ONLY
      NDX=ISVAV(LWD,IERR,MSER)                !GET INDEX VALUE
      IF(IERR.NE.0) RETURN                    !TST FOR ERROR
      IF(NDX.LT.1) GO TO 240                  !TST NDX VS LO-LIMIT
      IF(NDX.GT.ISYD(LDX)) GO TO 240          !TST NDX VS HI-LIMIT
      GO TO 100                               !GO PROCESS RIGHT SIDE
C
C     **************************************************************
C     LEFT SIDE IS OF FORM  -  SYM
C     **************************************************************
C
   50 CALL REFOR(IWD,LWD,ITYP,NF,IA,IEQ,NTER) !RE-FORMAT
      IF(NTER.NE.0) GO TO 210                 !TST FOR ERROR
      IF(NF.NE.1)   GO TO 210                 !REQUIRE 1 FIELD ONLY
      NDX=0                                   !NDX=0 FOR SIMPLE VAR
      NAMS(1)=LWD(1,1)                        !SAVE SYMBOL NAME
      NAMS(2)=LWD(2,1)                        !SAVE SYMBOL NAME
      LDX=LOCSYM(LWD(1,1))                    !FIND IT IN TABLE
      IF(LDX.EQ.0) GO TO 100                  !O.K. IF NOT IN TABLE
C                                             !WE WILL ADD IT
      IF(ISYT(LDX).NE.1) GO TO 250            !REQUIRE NON-INDEXED
C
C     **************************************************************
C     LDX = INDEX IN SYMBOL TABLE FOR SIMPLE OR SUBSCRIPTED VARIABLE
C     LDX = 0 SAYS SIMPLE VARIABLE BUT NOT IN TABLE - O.K. WE ADD
C     NDX = SYMBOL ELEMENT # (SUBSCRIPTED ONLY)
C     NDX = 0 FOR SIMPLE VARIABLE
C     **************************************************************
C     NOW PROCESS THE RIGHT HAND SIDE (EXPRESSION PART)
C     **************************************************************
C
  100 IS=IEQ+1                                !RESET SCAN PNTR
C
      ISUM=KVALU(IWD,IS,IB,IERR,MSER)
      IF(IERR.NE.0) RETURN
      IF(IERR.EQ.1000) GO TO 260
C
C     **************************************************************
C     NOW STORE RESULT FOR INDEXED OR SIMPLE VARIABLE
C     **************************************************************
C
      IF(NDX.EQ.0) GO TO 160                  !TST FOR SIMPLE VAR
C                                             !STORE INDEXED
      IDX=ISYP(LDX)+NDX-1                     !COMPUTE INDEX
      ISYV(IDX)=ISUM                          !STORE VALUE
      RETURN
C
  160 IF(LDX.EQ.0) GO TO 170                  !TST FOR EXIST
      ISYV(ISYP(LDX))=ISUM                    !YES, STORE VALUE
      RETURN
C
  170 NS=NSYM+1                               !NO, INC # SYMBOLS
      IF(NS.GT.MAXNS) GO TO 270               !TST FOR TOO MANY
      NV=NSYV+1                               !INC # VALUES
      IF(NV.GT.MAXNV) GO TO 280               !TST FOR TOO MANY
      ISYN(1,NS)=NAMS(1)                      !STORE NAME
      ISYN(2,NS)=NAMS(2)                      !STORE NAME
      ISYT(NS)=1                              !STORE TYPE
      ISYP(NS)=NV                             !SET VALUE PNTR
      ISYD(NS)=1                              !SET DIMENSION
      ISYV(NV)=ISUM                           !STORE VALUE
      NSYM=NS                                 !SAVE # SYMBOLS
      NSYV=NV                                 !SAVE # VALUES
      RETURN
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP ERROR MESSAGE
C     **************************************************************
C
  210 JJ=1
      GO TO 300
  220 JJ=2
      MSG(9,2) =LWD(1,1)
      MSG(10,2)=LWD(2,1)
      GO TO 300
  230 JJ=3
      MSG(9,3) =LWD(1,1)
      MSG(10,3)=LWD(2,1)
      GO TO 300
  240 JJ=4
      MSG(9,4) =LWD(1,1)
      MSG(10,4)=LWD(2,1)
      GO TO 300
  250 JJ=5
      MSG(9,5) =LWD(1,1)
      MSG(10,5)=LWD(2,1)
      GO TO 300
  260 JJ=6
      GO TO 300
  270 JJ=7
      GO TO 300
  280 JJ=8
C
  300 DO 310 I=1,10
      MSER(I)=MSG(I,JJ)
  310 CONTINUE
      IERR=JJ
      RETURN
      END
