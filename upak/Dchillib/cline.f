C$PROG CLINE
      SUBROUTINE CLINE(IWD,IERR,MSER)
C
      INTEGER*4 IWD(32),ICH(80),MSER(10),MSG(10,10)
C
      CHARACTER*40 MSC(10)
C
      INTEGER*4 LWD(2,40),ITYP(40)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/'SYNTAX ERROR - CLASSIFYING INPUT LINE   ',
     2         'TOO MANY "= SIGNS"                      ',
     3         'STATEMENT LABEL MORE THAN 4 CHARACTERS  ',
     4         'MORE THAN 8 CHARACTERS IN SYM OR # FIELD',
     5         'STATEMENT LABEL ILLEGAL FOR LINE TYPE   ',
     6         'DESTINATION LABEL MORE THAN 4 CHARACTERS',
     7         'PARENTHESIS DO NOT MATCH                ',
     8         'REFERENCED SUBROUTINE NAME UNRECOGNIZED ',
     9         'REPACK PARAMETER RANGE FIELD IS NULL    ',
     A         '                                        '/
C
      CHARACTER*4  KWD,LABLS,LABLG,KWDT(2),CTMP
C
      INTEGER*4    CKWD,ILABLS,ILABLG,ITMP
C
C
      EQUIVALENCE (CKWD,KWD),(ILABLS,LABLS),(ILABLG,LABLG),(CTMP,ITMP)
C
      INTEGER*4   DIR,C,DO,EQU,LC,H,OH,GOTO,CALL,BTAB,CONT,BLANK
      CHARACTER*4   cDIR,cC,cDO,cEQU,cLC,cH,cOH,cGOTO,cCALL,cBTAB,
     $              cCONT,cBLANK
      EQUIVALENCE (CDIR,DIR), (CC,C), (CDO,DO), (CEQU,EQU), (CLC,LC), 
     $            (CH,H), (COH,OH), (CGOTO,GOTO), (CCALL,CALL), 
     $            (CBTAB,BTAB), (CCONT,CONT), (CBLANK,BLANK)
C
      DATA       CDIR /'DIR '/
      DATA       CC   /'C   '/
      DATA       CDO  /'DO  '/
      DATA       CEQU /'EQU '/
      DATA       CLC  /'LC  '/
      DATA       CH   /'H   '/
      DATA       COH  /'OH  '/
      DATA       CGOTO/'GOTO'/
      DATA       CCALL/'CALL'/
      DATA       CBTAB/'BTAB'/
      DATA       CCONT/'CONT'/
      DATA      CBLANK/'    '/
C
      INTEGER*4   X20,X24,X28,X29,X3B,X3D,X42,X4C,X4F,X54
C
      DATA        X20/Z'20'/
      DATA        X24/Z'24'/
      DATA        X28/Z'28'/
      DATA        X29/Z'29'/
      DATA        X3B/Z'3B'/
      DATA        X3D/Z'3D'/
      DATA        X42/Z'42'/
      DATA        X4F/Z'4F'/
      DATA        X54/Z'54'/
C
      SAVE
C
C     **************************************************************
C     CLASSIFIES THE LINE CONTAINED IN (IWD(I),I=1,20) AND RECORDS
C     THE RESULTS IN (IWD(I),I=21,32)
C     **************************************************************
C
C                        IWD(23) - IWD(24)  BOUND
C                        (JLOB)    (JHIB)
C                        ------------------------
C     IWD(21) = 'ERR '   UNDEFINED
C             = 'DIR '   UNDEFINED
C             = 'EQU '   X=EXPRESSION
C             = 'IFU '   (CONDITION)
C             = 'IFS '   (CONDITION)
C             = 'IFX '   (CONDITION)
C             = 'IFN '   (CONDITION)
C             = 'IFC '   (CONDITION)
C             = 'IFP '   (CONDITION)
C             = 'ASP '   (IPAR,IVAL)
C             = 'BTAB'   (CONDITION)
C             = 'CALL'   USERSUBX  OR  REPACKX
C             = 'H   '   EVERYTHING BUT LABEL & COMMENT
C             = 'OH  '   EVERYTHING BUT LABEL & COMMENT
C             = 'LC  '   EVERYTHING BUT COMMENT
C             = 'GOTO'   DESTINATION
C             = 'DO  '   EVERYTHING BUT LABEL & COMMENT
C             = 'CONT'   NOT NEEDED
C
C     IWD(22) - CONTAINS ERROR CODE
C
C     IWD(23) - CONTAINS LO-BYTE TO SCAN
C
C     IWD(24) - CONTAINS HI-BYTE TO SCAN
C
C     IWD(25) - CONTAINS LEQ (LOCATION OF "=" IF ANY)
C
C     IWD(26) - CONTAINS STATEMENT LABEL IF ANY
C
C     IWD(27) - CONTAINS "DESTINATION LABEL", IF ANY
C
C     IWD(27) - CONTAINS "SOME DEST" FOR BTAB
C     IWD(28) - CONTAINS "NONE DEST" FOR BTAB
C     IWD(29) - CONTAINS "ALL  DEST" FOR BTAB
C     IWD(30) - CONTAINS LAST SOURCE LINE # (NSOL)
C
C     **************************************************************
C
      DO 10 I=21,32                     !RESET THE RESULTS PART
      IWD(I)=BLANK
   10 CONTINUE
C
      DO 20 I=23,25
      IWD(I)=0
   20 CONTINUE
C
      IERR=0
      LABLS='    '                      !BLANK STATEMENT LABEL
      LABLG='    '                      !BLANK GOTO      LABEL
      JLOB=0                            !ZERO LO-BYTE PNTR
      JHIB=0                            !ZERO HI-BYTE PNTR
      LEQ=0                             !ZERO = SIGN  PNTR
      NEQ=0                             !ZERO = SIGN  CNTR
C
C     **************************************************************
C     TST FOR $-DIRECTIVE TYPE
C     **************************************************************
      CALL ILBYTE(IT,IWD,0)             !PICK UP 1ST BYTE
      IF(IT.NE.X24) GO TO 50            !TST FOR "$"
C
      IWD(21)=DIR            !$DIR - $DIR - $DIR - $DIR - $DIR
      GO TO 800
C
C     **************************************************************
C     UNPACK THE LINE INTO ICH AND LOCATE LO-BYTE, HI-BYTE, =, ETC
C     **************************************************************
C
   50 DO 60 I=1,80                      !LOOP ON 80 BYTES
      CALL ILBYTE(ICH(I),IWD,I-1)       !LOAD BYTE INTO ICH
      IF(ICH(I).EQ.X3B) GO TO 65        !TST FOR "!" (DONE IF FOUND)
      IF(ICH(I).EQ.X20) GO TO 60        !TST FOR BLANK
      JHIB=I                            !OTHERWISE, RESET HI-BYTE
      IF(JLOB.LE.0) JLOB=0              !RESET LO-BYTE IF NOT DONE
      IF(ICH(I).NE.X3D) GO TO 60        !TST FOR "="
      NEQ=NEQ+1                         !INC   = CNTR
      LEQ=I                             !RESET = PNTR
   60 CONTINUE
   65 IF(JHIB.GT.0) GO TO 70
      IWD(21)=C               !COMMENT - COMMENT - COMMENT - COMMENT
      GO TO 800
C
   70 IF(NEQ.GT.1) GO TO 1020           !TST FOR TOO MANY = SIGNS
C
      N=0
      IA=NXNB(IWD,1,5)                  !1ST BYTE OF STATEMENT LABEL
      IF(IA.LE.0) GO TO 85              !TST FOR BLANK
      IB=LSNB(IWD,IA,5)                 !LAST BYTE OF LABEL
      DO 80 I=IA,IB                     !LOAD UP ANY STATEMENT LABEL
      IF(ICH(I).EQ.X20) GO TO 1010      !ERROR IF IMBEDED BLANK
      N=N+1                             !INC BYTE CNTR
      IF(N.GT.4) GO TO 1030             !TST FOR TOO MANY BYTES
      CALL ISBYTE(ICH(I),LABLS,N-1)     !LOAD INTO LABEL
   80 CONTINUE
C
   85 IF(NEQ.LE.0) GO TO 150            !TST FOR = SIGN FOUND
C
C     **************************************************************
C     WE HAVE AN = SIGN - CLASSIFY AS "DO" OR "EQUATE"
C     **************************************************************
C
      IF(LEQ.LT.8) GO TO 1010           !TST FOR ERROR
      IF(ICH(6).NE.X20) GO TO 1010      !TST FOR ERROR
      JLOB=NXNB(IWD,7,JHIB)             !RESET LO-BYTE PNTR
      IF(JLOB.LE.0) GO TO 1010
C
      DO 90 I=JLOB,LEQ                  !LOOK FOR "(" - INDICATES EQU
      IF(ICH(I).EQ.X28) GO TO 110
   90 CONTINUE
C
      CALL REFOR(IWD,LWD,ITYP,NF,JLOB,LEQ,NTER)
      IF(NTER.NE.0) GO TO 1040          !TST FOR TRUNCATION ERROR
      IF(LWD(1,1).NE.DO)     GO TO 110  !TST FOR "DO"
      IF(NF.NE.3)            GO TO 110  !TST FOR 3 FIELDS
C
      N=0                               !IT LOOKS LIKE A "DO"
      DO 100 I=1,8                      !LOAD UP END-LOOP LABEL
      CALL ILBYTE(IT,LWD(1,2),I-1)      !PICK UP BYTE
      IF(IT.EQ.X20) GO TO 100           !SKIP BLANKS
      N=N+1                             !INC BYTE CNTR
      IF(N.GT.4) GO TO 1030             !TST FOR TOO MANY
      CALL ISBYTE(IT,LABLG,N-1)         !LOAD BYTE INTO LABEL
  100 CONTINUE
C
      IWD(21)=DO              !DO - DO - DO - DO - DO - DO - DO - DO
      GO TO 800
C
C     **************************************************************
C     CLASSIFY AS AN EQUATE
C     **************************************************************
C
  110 IWD(21)=EQU             !EQUATE - EQUATE - EQUATE - EQUATE ---
      GO TO 800
C
  150 IF(ICH(6).EQ.X20) GO TO 160       !TST FOR CONTINUATION
      IF(LABLS.NE.'    ') GO TO 1050
      IWD(21)=LC              !CONTINUATION - CONTINUATION ---------
C
      JLOB=7
      DO 155 I=7,JHIB
      IF(ICH(I).EQ.X20) GO TO 155
      JLOB=I
      GO TO 800
  155 CONTINUE
      GO TO 1010
C
  160 KWD='    '                        !BLANK TST WD
      DO 170 I=7,JHIB                   !FIND FIRST NON-BLANK
      IF(ICH(I).NE.X20) GO TO 180
  170 CONTINUE
      GO TO 1010                        !ERROR IF ALL BLANK
C
C     **************************************************************
C     USE THE NITTY-GRITTY APPROACH - PICK UP BYTE, TST, ETC, ETC
C     **************************************************************
C
  180 JLOB=I
      IA=JLOB
      N=0
      CALL ISBYTE(ICH(IA),KWD,N)
      IF(KWD.NE.'H   ') GO TO 190
      IT=NXNB(IWD,IA+1,JHIB)
      IF(IT.LE.0) GO TO 1010
      IF(ICH(IT).NE.X28) GO TO 1010
      IWD(21)=H               !H  -  H  -  H  -  H  -  H  -  H  -  H
      GO TO 800
C
C     **************************************************************
C
  190 IA=IA+1
      N=N+1
      CALL ISBYTE(ICH(IA),KWD,N)
      IF(KWD.NE.'OH  ') GO TO 200
      IT=NXNB(IWD,IA+1,JHIB)
      IF(IT.LE.0) GO TO 1010
      IF(ICH(IT).NE.X28) GO TO 1010
      IWD(21)=OH              !OH  -  OH  -  OH  -  OH  -  OH  -  OH
      GO TO 800
C
C     **************************************************************
C
  200 IF(KWD.NE.'GO  ') GO TO 210
      IT=NXNB(IWD,IA+1,JHIB)
      IF(IT.LE.0) GO TO 1010
      IF(ICH(IT).NE.X54) GO TO 1010
      IF(ICH(IT+1).NE.X4F) GO TO 1010
      IA=NXNB(IWD,IT+2,JHIB)            !PICK UP DEST (LABEL)
      IF(IA.LE.0) GO TO 1010
      JLOB=IA
      N=0
      DO 202 I=IA,JHIB
      IF(ICH(I).EQ.X20) GO TO 204
      IF(N.GT.3) GO TO 1010
      CALL ISBYTE(ICH(I),LABLG,N)
      N=N+1
  202 CONTINUE
C
  204 IWD(21)=GOTO            !GOTO - GOTO - GOTO - GOTO - GOTO ----
      GO TO 800
C
C     **************************************************************
C
  210 IA=IA+1
      N=N+1
      CALL ISBYTE(ICH(IA),KWD,N)
      IF(KWD.NE.'CAL ') GO TO 220
      IF(ICH(IA+1).NE.X4C) GO TO 1010
      JLOB=NXNB(IWD,IA+2,JHIB)
      IF(JLOB.LE.0) GO TO 1010
      JHI=JLOB+7
      N=0
C
      DO 212 I=JLOB,JHI
      CALL ISBYTE(ICH(I),KWDT,N)
      N=N+1
  212 CONTINUE
C
      IF(KWDT(1).EQ.'REPA') GO TO 215
      IF(KWDT(1).NE.'USER') GO TO 1080
      IF(KWDT(2).EQ.'SUB1') GO TO 214
      IF(KWDT(2).EQ.'SUB2') GO TO 214
      IF(KWDT(2).EQ.'SUB3') GO TO 214
      GO TO 1080
C
  214 LABLG=KWDT(2)
      IWD(21)=CALL            !CALL - CALL - CALL - CALL - CALL ----
      GO TO 800
C
  215 IF(KWDT(2).EQ.'CK1 ') GO TO 216
      IF(KWDT(2).EQ.'CK2 ') GO TO 216
      IF(KWDT(2).EQ.'CK3 ') GO TO 216
      GO TO 1080
C
  216 LABLG=KWDT(2)
      IWD(21)=CALL
      IA=NXNB(IWD,JHI+1,JHIB)
      IF(IA.LE.0) GO TO 1090
      IB=LSNB(IWD,IA,JHIB)
      IF(IB.LE.0) GO TO 1090
      JLOB=IA
      JHIB=IB
      GO TO 800
C
C     **************************************************************
C
  220 IF(KWD.NE.'BTA ') GO TO 230
      IF(ICH(IA+1).NE.X42) GO TO 1010
      IA=IA+2
      IA=IFIND(IWD,X28,IA,JHIB)          !FIND "("
      IF(IA.LE.0) GO TO 1010
      JLOB=IA+1
      NLP=1
      NRP=0
      LRP=0
C
      DO 222 I=JLOB,JHIB
      IF(ICH(I).EQ.X28) NLP=NLP+1
      IF(ICH(I).NE.X29) GO TO 222
      NRP=NRP+1
      LRP=I
  222 CONTINUE
      IF(NLP.NE.NRP) GO TO 1070
      IA=LRP+1
C
      CALL REFOR(IWD,LWD,ITYP,NF,IA,JHIB,NTER)
      IF(NTER.NE.0) GO TO 1040
C
      DO 226 J=1,3
      N=0
      DO 224 I=1,8
      CALL ILBYTE(IT,LWD(1,J),I-1)
      IF(IT.EQ.X20) GO TO 224
      IF(N.GT.3) GO TO 1060
      ITMP=IWD(J+26)
      CALL ISBYTE(IT,CTMP,N)
      N=N+1
  224 CONTINUE
  226 CONTINUE
C
C     CHANGE DEST-ORDER FROM - ALL,SOME,NONE  TO  SOME,NONE,ALL
C
      IALL=IWD(27)
      ISOM=IWD(28)
      INON=IWD(29)
      IWD(27)=ISOM
      IWD(28)=INON
      IWD(29)=IALL
C
      IWD(21)=BTAB            !BTAB - BTAB - BTAB - BTAB - BTAB ----
      IWD(22)=IERR
      IWD(23)=JLOB
      IWD(24)=LRP-1
      IWD(26)=ILABLS
      RETURN
C
C     **************************************************************
C
  230 IT=NXNB(IWD,IA+1,JHIB)
      IF(IT.LE.0) GO TO 1010
      IF(ICH(IT).NE.X28) GO TO 280
      JLOB=IT+1
      NLP=1
      NRP=0
      LRP=0
C
      DO 232 I=JLOB,JHIB
      IF(ICH(I).EQ.X28) NLP=NLP+1
      IF(ICH(I).NE.X29) GO TO 232
      NRP=NRP+1
      LRP=I
  232 CONTINUE
      IF(NLP.NE.NRP) GO TO 1070
C
      IA=LRP+1
      IB=JHIB
      JHIB=LRP-1
C
      IF(KWD.EQ.'ASP ') GO TO 240
      IF(KWD.EQ.'IFC ') GO TO 240
      IF(KWD.EQ.'IFP ') GO TO 240
C
      N=0
      DO 234 I=IA,IB
      IF(ICH(I).EQ.X20) GO TO 234
      N=N+1
      IF(N.GT.4) GO TO 1060
      CALL ISBYTE(ICH(I),LABLG,N-1)
  234 CONTINUE
C
      IF(KWD.EQ.'IFS ') GO TO 240
      IF(KWD.EQ.'IFU ') GO TO 240
      IF(KWD.EQ.'IFX ') GO TO 240
      IF(KWD.EQ.'IFN ') GO TO 240
      GO TO 1010
C
  240 IWD(21)=CKWD             !IFS - IFU - IFX - IFN - IFC - IFP ---
      GO TO 800
C
C     **************************************************************
C
  280 KWDT(1)='    '
      KWDT(2)='    '
      JHI=JLOB+7
      N=0
C
      DO 290 I=JLOB,JHI
      CALL ISBYTE(ICH(I),KWDT,N)
      N=N+1
  290 CONTINUE
      IF(KWDT(1).NE.'CONT') GO TO 1010
      IF(KWDT(2).NE.'INUE') GO TO 1010
      IF(LABLS.EQ.'    ')   GO TO 1010
      IWD(21)=CONT            !CONTINUE - CONTINUE - CONTINUE ------
C
C     **************************************************************
C
C
  800 IWD(22)=IERR                      !STORE ERROR CODE
      IWD(23)=JLOB                      !STORE LO-BYTE TO SCAN
      IWD(24)=JHIB                      !STORE HI-BYTE TO SCAN
      IWD(25)=LEQ                       !STORE = LOCATION
      IWD(26)=ILABLS                    !STORE STATEMENT LABEL
      IWD(27)=ILABLG                    !STORE GOTO      LABEL
      RETURN
C
 1010 IERR=1
      GO TO 1200
 1020 IERR=2
      GO TO 1200
 1030 IERR=3
      GO TO 1200
 1040 IERR=4
      GO TO 1200
 1050 IERR=5
      GO TO 1200
 1060 IERR=6
      GO TO 1200
 1070 IERR=7
      GO TO 1200
 1080 IERR=8
      GO TO 1200
 1090 IERR=9
      GO TO 1200
C1100 IERR=10
C
 1200 DO 1210 I=1,10
      MSER(I)=MSG(I,IERR)
 1210 CONTINUE
      RETURN
      END
