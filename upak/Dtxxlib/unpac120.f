C$PROG UNPAC120
C
      SUBROUTINE UNPAC120(IWD,ICH,NUM)
C
      INTEGER*4 ICH(120,2),IWD(1),JTEMP
C
      CHARACTER*4   JTC
C
      EQUIVALENCE   (JTC,JT)
C
      INTEGER*4     X5C,X20
C
      DATA          X5C,X20/'5C'X,'20'X/
C
      INTEGER*4     TST1,TST2,TST3,TST4,TST5
C
      DATA          TST1/'205C425C'X/
      DATA          TST2/'205C555C'X/
      DATA          TST3/'5C55425C'X/
      DATA          TST4/'5C42555C'X/
      DATA          TST5/'20205C5C'X/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     ROUTINE TO UNPACK BYTES FROM IWD INTO LO-ORDER BYTES IN ICH
C
      N=0
      I=0
      IF=0
   10 N=N+1
      IF(N.GT.NUM) GO TO 100
      CALL ILBYTE(IT,IWD,N-1)
      IF(IT.NE.X5C) GO TO 50
      IHI=IFIND(IWD,IT,N+1,N+4)
      IF(IHI.LE.0) GO TO 50
      JTC='    '
      CALL LODUP(IWD,N,IHI,JT,1)
C
      CALL CASEUP4(JT)
C
      IF(JT.EQ.TST1) IF=1     !\B\
      IF(JT.EQ.TST2) IF=2     !\U\
      IF(JT.EQ.TST3) IF=3     !\BU\
      IF(JT.EQ.TST4) IF=3     !\UB\
      IF(JT.EQ.TST5) IF=-1    !\\
C
C     IF(JTC.EQ.'\B\ ') IF=1
C     IF(JTC.EQ.'\U\ ') IF=2
C     IF(JTC.EQ.'\BU\') IF=3
C     IF(JTC.EQ.'\UB\') IF=3
C     IF(JTC.EQ.'\\  ') IF=-1
C
      NAD=IHI-N
      N=N+NAD
C
      IF(IF.EQ.-1)     THEN
                       ICH(I,2)=IF
                       IF=0
                       ENDIF
      GO TO 10
C
   50 I=I+1
      ICH(I,1)=IT
      IF(IF.EQ.0) ICH(I,2)=0
      IF(IF.NE.0) THEN
                  ICH(I,2)=IF
                  IF=0
                  ENDIF
      GO TO 10
C
  100 IF(I.GE.NUM) RETURN
      DO 110 J=I,NUM
      ICH(J,1)=X20
      ICH(J,2)=0
  110 CONTINUE
C
      RETURN
      END
