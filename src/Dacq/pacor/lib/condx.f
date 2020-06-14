C$PROG CONDX
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE CONDX(IDONE,IERR)
C
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
C
      COMMON/PAC7/ ISORL,IEXPL,LISTYP,NERR
C
      BYTE IBY(80)
C
      INTEGER*4     LWD3(3,40)
C
      CHARACTER*4  CLWD3(3,40),CLWD(2,40),CIwD(20)
C
      EQUIVALENCE (CLWD3,LWD3),(CLWD,LWD),(CIWD,IWD)
C
      EQUIVALENCE (IBY,IWD)
C
      character*4 cISETW
      equivalence (cISETW, ISETW)
      DATA cISETW,NCALL/'SETW',0/
C
      CHARACTER*4  IDONE
C
      SAVE
C
C     ************************************************************
C     WRITES LOOP-EXPANDED & RE-FORMATTED CONDITIONAL READOUT CODE
C     ON TMP-FILE FOR SUBSEQUENT PROCESSING BY CONDICO 
C     ************************************************************
C
      IF(NCALL.GT.0) GO TO 50
C
      WRITE(8,10)ISORL,IEXPL                    !STUFF IN CONTINUE
   10 FORMAT(6X,'CONT',56X,2I6)                 !AT BEGINNING
      NCALL=1
C
   50 IERR=0
C
      CALL GREAD(IWD,LWD3,ITYP,NF,ISETW,12,NTER)
C
      CALL GREAD(IWD,LWD3,ITYP,NF,7,80,NTER)
C
      CALL GREAD(IWD,LWD3,ITYP,NF,ISETW,8,NTER)
C
      IF(CLWD3(1,1).EQ.'ENDL') THEN
                               CLWD3(1,1)='CONT'
                               CLWD3(2,1)='    '
                               ENDIF
      IF(CLWD3(1,1).EQ.'LOOP') THEN
                               CLWD3(1,1)='CONT'
                               CLWD3(2,1)='    '
                               ENDIF
      IF(CLWD3(1,1).EQ.'CONT') CLWD3(2,1)='    '
C
      IF(CIWD(1).EQ.'    ')    GO TO 100
      IF(CLWD3(1,1).NE.'READ') GO TO 100

      WRITE(8,60)IWD(1),ISORL,IEXPL
   60 FORMAT(A4,2X,'CONT',56X,2I6)
      CIWD(1)='    '
C 
  100 IF(CLWD(1,1).NE.'CNAF') GO TO 110
      CALL LWDMOD
      WRITE(8,105)IWD(1),LWD(1,1),((LWD(I,J),I=1,2),J=2,6),
     &                                          ISORL,IEXPL
  105 FORMAT(2(A4,2X),2A4,6X,4(2A4,2X),2I6)
      GO TO 500
C
  110 IF(CLWD(1,1).NE.'    ') THEN
C
      CALL LWDMOD3(LWD3)
C
      IF(CLWD3(3,3).NE.'    ') THEN
                               CLWD3(1,3)=CLWD3(2,3)
                               CLWD3(2,3)=CLWD3(3,3)
                               ENDIF
C
      IF(CLWD3(3,4).NE.'    ') THEN
                               CLWD3(1,4)=CLWD3(2,4)
                               CLWD3(2,4)=CLWD3(3,4)
                               ENDIF 
C
      WRITE(8,120)IWD(1),LWD3(1,1),
     &                   (LWD3(I,2),I=1,3),
     &                  ((LWD3(I,J),I=1,2),J=3,6),
     &                    ISORL,IEXPL
                             ENDIF
  120 FORMAT(2(A4,2X),3A4,2X,4(2A4,2X),2I6)
C
  500 IDONE='YES '
      RETURN
      END
