C$PROG LABLST
      SUBROUTINE LABLST(IWD,IA,IB,LABLS,NLA,IERR,MSER)
C
      INTEGER*4    IWD(*),MSG(10,3),MSER(10)
C
      CHARACTER*4  LABLS(100)
C
      CHARACTER*40 MSC(3)
C
      EQUIVALENCE (MSC(1),MSG(1,1))
C
      DATA MSC/'MORE THAN 100 "GO TO" LABELS            ',
     2         'MORE THAN 4 CHARACTERS IN "GO TO" LABEL ',
     3         'NULL "GO TO" LABEL FIELD                '/
C
      INTEGER*4    X20,X29,X2C,X3B
      DATA         X20,X29,X2C,X3B/z'20',z'29',z'2C',z'3B'/
C
      SAVE
C
C     **************************************************************
C     EXTRACTS LIST OF 4-CHAR LABELS FROM "IWD" & STORES IN "LABLS"
C     IA THRU IB = RANGE IN IWD TO SCAN
C     NLA        = NUMBER OF LABELS RETURNED
C     **************************************************************
C
      IERR=0                            !RESET ERROR FLAG
      NLA=0                             !RESET # OF LABELS
      II=IA-1                           !INIT SCAN PNTR
C
   10 II=II+1                           !INC SCAN PNTR
      IF(II.GT.IB) GO TO 50             !TST FOR DONE
C
      CALL ILBYTE(IT,IWD,II-1)          !PICK UP NEXT BYTE
      IF(IT.EQ.X20) GO TO 10            !TST FOR BLANK
      IF(IT.EQ.X29) GO TO 10            !TST FOR - )
      IF(IT.EQ.X2C) GO TO 10            !TST FOR - ,
      IF(IT.EQ.X3B) GO TO 50            !TST FOR - ! (DONE)
C
      NLA=NLA+1                         !INC # LABELS CNTR
      IF(NLA.GT.100) GO TO 110          !TST FOR TOO MANY
      LABLS(NLA)='    '                 !BLANK OUT LABEL
      CALL ISBYTE(IT,LABLS(NLA),0)      !STORE FIRST BYTE
      JA=II+1                           !INIT IWD-PNTR
      NC=1                              !# OF LABEL BYTES LOADED
      DO 20 J=JA,IB                     !LOOP TO LOAD LABEL
      II=II+1                           !INC SCAN PNTR
      CALL ILBYTE(IT,IWD,II-1)          !PICK UP BYTE
      IF(IT.EQ.X2C) GO TO 10            !TST FOR - ,
      IF(IT.EQ.X20) GO TO 10            !TST FOR BLANK
      IF(IT.EQ.X3B) GO TO 50            !TST FOR - !  (DONE)
      NC=NC+1                           !INC LABEL BYTE CNTR
      IF(NC.GT.4) GO TO 120             !TST FOR TOO MANY
      CALL ISBYTE(IT,LABLS(NLA),NC-1)   !LOAD LABEL BYTE
   20 CONTINUE
      GO TO 10                          !GO BACK FOR MORE
C
   50 IF(NLA.LE.0) GO TO 130            !TST FOR NO LABELS FOUND
      RETURN
C
  110 IERR=1
      GO TO 200
  120 IERR=2
      GO TO 200
  130 IERR=3
C
  200 DO 210 I=1,10
      MSER(I)=MSG(I,IERR)
  210 CONTINUE
      RETURN
      END
