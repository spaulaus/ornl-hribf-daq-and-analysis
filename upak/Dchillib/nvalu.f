C$PROG NVALU
      FUNCTION NVALU(NAME,IERR,MSER)
C
      CHARACTER*8   NAME
C
      CHARACTER*40  MSER
C
      CHARACTER*40  MSC(2)
C
      DATA MSC/
     1'ILLEGAL CHARACTER IN INTEGER FIELD      ',
     2'ILLEGAL CHARACTER IN HEXADECIMAL FIELD  '/
C
       INTEGER*4   X20,X30,X39,X41,X46,X48
C
       DATA        X20/Z'20'/
       DATA        X30/Z'30'/
       DATA        X39/Z'39'/
       DATA        X41/Z'41'/
       DATA        X46/Z'46'/
       DATA        X48/Z'48'/
C
       SAVE
C
C     **************************************************************
C     DECODES RIGHT-JUSTIFIED INTEGER OR HEX ASCII FIELD (8 BYTES)
C     **************************************************************
C
      IERR=0
      CALL ILBYTE(JT,NAME,7)                   !TST LAST BYTE
      IF(JT.EQ.X48) GO TO 100                  !IF "H", ASSUME HEX
C
      DO 10 I=1,8                              !LOOP ON ALL BYTES
      CALL ILBYTE(JT,NAME,I-1)                 !GET BYTE
      IF(JT.EQ.X20) GO TO 10                   !TST FOR BLANK
      IF(JT.GE.X30.AND.JT.LE.X39) GO TO 10     !TST FOR NUMERIC
      GO TO 210                                !OTHERWISE, ERROR
   10 CONTINUE
      READ(NAME,20)IV                          !DECODE INTEGER FIELD
   20 FORMAT(I8)
      NVALU=IV                                 !SET FCN VALUE
      RETURN
C
  100 DO 110 I=1,7                             !LOOP ON 1ST 7 BYTES
      CALL ILBYTE(JT,NAME,I-1)                 !GET BYTE
      IF(JT.EQ.X20) GO TO 110                  !TST FOR BLANK
      IF(JT.GE.X30.AND.JT.LE.X39)GO TO 110     !TST FOR NUMERIC
      IF(JT.GE.X41.AND.JT.LE.X46)GO TO 110     !TST FOR HEX
      GO TO 220                                !OTHERWISE, ERROR
  110 CONTINUE
      READ(NAME,120)IV                         !DECODE HEX FIELD
  120 FORMAT(Z7)
      NVALU=IV
      RETURN
C
  210 JJ=1
      GO TO 300
  220 JJ=2
      GO TO 300
C
  300 MSER=MSC(JJ)
      IERR=JJ
      NVALU=0
      RETURN
      END
