C$PROG LABLOOP
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 03/18/92
C     ************************************************************
C
      SUBROUTINE LABLOOP(MODE,JWD)
C
      INTEGER*4 LABL(1000),LABV(1000)
C
      INTEGER*4 JWD(20),IWD(20),LWD(2,40),ITYP(40),NF
C
      CHARACTER*8  CDUM
      INTEGER*4    IDUM(2)
      EQUIVALENCE (CDUM,IDUM)
C
      DATA KLAB,NLAB/0,0/
C
      CHARACTER*4   MODE
C
      CHARACTER*5  CIWD
C
      CHARACTER*4   CLWD(2,40)
C
      EQUIVALENCE  (CLWD,LWD),(CIWD,IWD(1))
C
      INTEGER*4     BLANK
      character*4   cBLANK
      equivalence   (cBLANK, BLANK)
      DATA          cBLANK/'    '/
C
      SAVE
C
C     ************************************************************
C     CREATES PSEUDO-LABELS IN SUPPORT OF LOOP EXPANDER - LOOPEX
C     ************************************************************
C
      DO 10 I=1,20
      IWD(I)=JWD(I)
   10 CONTINUE
C
      IF(MODE.EQ.'INIT') GO TO 100
      IF(MODE.EQ.'SAVE') GO TO 200
      IF(MODE.EQ.'SET ') GO TO 300
      IF(MODE.EQ.'SUB ') GO TO 400
                         GO TO 500
C
  100 NLAB=0                                !SET # OF LABELS TO ZERO
      GO TO 500
C
  200 IF(IWD(1).EQ.BLANK) GO TO 500         !SAVE LINE LABEL IN LABL
      NLAB=NLAB+1
      LABL(NLAB)=IWD(1)
      GO TO 500
C
  300 DO 310 I=1,NLAB                       !ASSIGN SEQUENTIAL INTEGER
      KLAB=KLAB+1                           !VALUE TO ALL LABELS SAVED
      LABV(I)=KLAB
  310 CONTINUE
      GO TO 500

C
  400 IT=IWD(1)                             !SUBSTITUTE NUMERICAL :XXX
      IF(IT.EQ.BLANK) GO TO 430             !LABEL FOR ALL LINE-LABELS
      DO 410 I=1,NLAB                       !AND DESTINATION LABELS IN
      IF(IT.EQ.LABL(I)) GO TO 420           !LIST
  410 CONTINUE
      GO TO 430
C
  420 WRITE(CIWD,425)LABV(I)                !BUILD LINE-LABEL
  425 FORMAT(':',I4)
      CALL SQUEZL(IWD,1,5)                  !AND INSERT IT 
C
  430 CALL GREAD(IWD,LWD,ITYP,NF,7,80,NTER) !RE-FORMAT INPUT LINE
C
      IF(CLWD(1,1).EQ.'IFU ') GO TO 440     !TST FOR DESTINATION FIELS
      IF(CLWD(1,1).EQ.'IFS ') GO TO 440
      IF(CLWD(1,1).EQ.'IFA ') GO TO 440
      IF(CLWD(1,1).EQ.'IFN ') GO TO 440
      IF(CLWD(1,1).EQ.'IFT ') GO TO 440
      IF(CLWD(1,1).EQ.'IFF ') GO TO 440
      IF(CLWD(1,1).EQ.'GOTO') GO TO 440
      GO TO 500                             !IF NOT RETURN
C
  440 DO 445 I=1,NLAB                       !OTHERWISE, LOOK FOR DEST
      IF(LWD(1,NF).EQ.LABL(I)) GO TO 450    !IN LABEL LIST AND REPLACE
  445 CONTINUE
      GO TO 500
C
  450 WRITE(CDUM,455) LABV(I)               !WITH GENERATED SYMBOL
  455 FORMAT(':',I6,'!')                    !AND LINE TERMINATOR
C
      CALL SQUEZL(IDUM,1,8)                 !SQUEEZE LEFT
C
      IL=LASWD(IWD,7,80)                    !LOCATE WORD TO REPLACE
      IF(IL.LE.0) RETURN                    !THIS IS AN ERROR!!!
C
      CALL LODUP(IDUM,1,8,IWD,IL)           !LOAD IT IN SOURCE LINE
C
  500 DO 510 I=1,20
      JWD(I)=IWD(I)
  510 CONTINUE
      RETURN
      END
