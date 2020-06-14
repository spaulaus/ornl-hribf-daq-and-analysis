C$PROG ASSYM
      SUBROUTINE ASSYM(IERR,MSER)
C
      COMMON/AAA/ IWD(20),LWD(2,40),ITYP(40),NF
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
C
      COMMON/FFF/ LIST(2002),LPAR(2002),IPSP(2002),IPSI(2002),
     &            MXPAR,NPAR,MINIP,MAXIP,LPARF
C
      INTEGER*4    JLST(40)
      INTEGER*4    MSG(10,10),MSER(10)
      CHARACTER*40 MSC(10)
      CHARACTER*4  CIWD1
C
      EQUIVALENCE (CIWD1,IWD(1))
C
      EQUIVALENCE (JLO,JLST(1)),(JNC,JLST(2))
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA JLST/2*1,38*0/
C
      DATA MSC/
     1'SYNTAX ERROR - "= SIGN" NOT FOUND       ',
     2'SYNTAX ERROR - LEFT SIDE OF "= SIGN"    ',
     3'WRONG # OF FIELDS - LEFT OF "= SIGN"    ',
     4'WRONG FIELD TYPE - LEFT OF "= SIGN"     ',
     5'SYMBOL NOT FOUND IN TABLE               ',
     6'ILLEGAL INDEX OR SYNTAX ERROR           ',
     7'SYNTAX ERROR - RIGHT OF "= SIGN"        ',
     8'WRONG NUMBER OF FIELDS - RIGHT OF "="   ',
     9'WRONG FIELD TYPE - RIGHT OF "= SIGN"    ',
     A'ILLEGAL VALUE - RIGHT OF "= SIGN"       '/
C
      DATA MXV/65535/
C
      SAVE
C
C     **************************************************************
C     PROCESS SYMBOL VALUE ASSIGNMENTS OF THE FORM:
C
C     $ASS SYM(ILO TO IHI,INC) = JLO,JNC
C     $ASS SYM(ILO TO IHI)     = JLO,JNC
C     $ASS SYM(IX1,IX2...)     = JLO
C     **************************************************************
C
      IERR=0
C
      IB=IFIND(IWD,Z'3D',5,80)                !LOOK FOR "="
      IF(IB.LE.0)       GO TO 210             !TST FOR ERROR
C
C     **************************************************************
C     PROCESS THE  -  SYM(ILO TO IHI,INC)  -  PART  (LEFT-HAND SIDE)
C     **************************************************************
C
      CALL REFOR(IWD,LWD,ITYP,NF,5,IB,NTER)   !REFORMAT LEFT-HAND PART
      IF(NTER.NE.0)     GO TO 220             !TST FOR ERROR
      IF(ITYP(1).NE.1)  GO TO 240             !REQUIRE FIELD-1 ALPHA
C
      CALL PLIST(LWD(1,2),ITYP(2),NF-1,LIST,NL,IERR,MSER)
      IF(IERR.NE.0) RETURN
C
      NDX=LOCSYM(LWD(1,1))                    !TST FOR SYMBOL EXIST
      IF(NDX.LE.0)      GO TO 250             !ERROR IF NOT
      LEN=ISYD(NDX)                           !GET ASSOCIATED LENGTH
C
      DO 20 I=1,NL                            !LOOP ON LIST VALUES
      IF(LIST(I).LT.1)   GO TO 260            !TST FOR LEGAL
      IF(LIST(I).GT.LEN) GO TO 260            !TST FOR LEGAL
   20 CONTINUE
C
      IA=IB+1                                 !INC SCAN POINTER
C
      IF(CIWD1.EQ.'$DAT') GO TO 100           !TST FOR EXPLICIT LIST
C
C     **************************************************************
C     PROCESS $ASS FORM  -  JLO,JNC  -  PART   (RIGHT-HAND SIDE)
C     **************************************************************
C
      CALL REFOR(IWD,LWD,ITYP,NF,IA,80,NTER)  !REFORMAT RIGHT-HAND PART
C
      IF(NTER.NE.0)     GO TO 270             !TST FOR ERROR
      IF(NF.LT.1)       GO TO 280             !REQUIRE 1 FIELD MIN
      IF(NF.GT.2)       GO TO 280             !REQUIRE 2 FIELDS MAX
C
      DO 40 J=1,NF                            !LOOP ON # FIELDS
      IF(ITYP(J).NE.2)  GO TO 290             !REQUIRE NUMERIC FIELD
      CALL LIMIV(LWD(1,J),0,MXV,JLST(J),JERR) !GET VALUE
      IF(JERR.NE.0)     GO TO 300             !TST FOR ERROR
   40 CONTINUE
      NJ=NF                                   !SAVE # J-VALUES
C
      IF(NJ.LT.2) JNC=0                       !DEFAULT RIGHT-SIDE INC
      JV=JLO                                  !JV = 1ST VALUE TO SET
      NOF=ISYP(NDX)-1                         !GET OFFSET IN ISYV
C
C     LOAD VALUES INTO ARRAY ISYV  *********************************
C
      DO 50 I=1,NL                            !LOOP ON LIST VALUES
      NDX=LIST(I)+NOF                         !GET INDEX
      ISYV(NDX)=JV                            !STORE VALUE
      JV=JV+JNC                               !INC VALUE TO STORE
   50 CONTINUE
      RETURN
C
C     **************************************************************
C     PROCESS $DAT FORM  -  EXPLICIT LIST PART (RIGHT-HAND SIDE)
C     **************************************************************
C
  100 CALL REFOR(IWD,LWD,ITYP,NF,IA,80,NTER)  !REFORMAT RIGHT-HAND PART
C
      IF(NTER.NE.0)     GO TO 270             !TST FOR ERROR
      IF(NF.NE.NL)      GO TO 280             !# ELEMENTS MUST MATCH
C
      DO 140 J=1,NF                           !LOOP ON # FIELDS
      IF(ITYP(J).NE.2)  GO TO 290             !REQUIRE NUMERIC FIELD
      CALL LIMIV(LWD(1,J),0,MXV,JLST(J),JERR) !GET VALUE
      IF(JERR.NE.0)     GO TO 300             !TST FOR ERROR
  140 CONTINUE
C
      NOF=ISYP(NDX)-1                         !GET OFFSET IN ISYV
C
C     LOAD VALUES INTO ARRAY ISYV  *********************************
C
      DO 150 I=1,NL                           !LOOP ON LIST VALUES
      NDX=LIST(I)+NOF                         !GET INDEX
      ISYV(NDX)=JLST(I)                       !STORE VALUE
  150 CONTINUE
      RETURN
C
C     SET ERROR TYPE AND RETURN  ***********************************
C
C     **************************************************************
C     SET ERROR CODE AND LOAD UP ERROR MESSAGE
C     **************************************************************
C
  210 JJ=1
      GO TO 400
  220 JJ=2
      GO TO 400
  240 JJ=4
      GO TO 400
  250 JJ=5
      GO TO 400
  260 JJ=6
      GO TO 400
  270 JJ=7
      GO TO 400
  280 JJ=8
      GO TO 400
  290 JJ=9
      GO TO 400
  300 JJ=10
C
  400 DO 410 I=1,10
      MSER(I)=MSG(I,JJ)
  410 CONTINUE
      IERR=JJ
      RETURN
      END
