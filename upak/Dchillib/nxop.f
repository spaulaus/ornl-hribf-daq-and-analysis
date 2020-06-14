C$PROG NXOP
      FUNCTION NXOP(IWD,IA,IB,KNOP)
C
      INTEGER*4 KOP(4),IWD(1)
C
      DATA KOP/z'2B',z'2D',z'2A',z'2F'/
C
      SAVE
C
C     **************************************************************
C     NXOP = LOCATION OF NEXT OPERATOR   +  -  *  /
C     KNOP = KIND OF OPERATOR            1  2  3  4
C     **************************************************************
C
      DO 30 I=IA,IB
      CALL ILBYTE(IT,IWD,I-1)
      DO 10 J=1,4
      IF(IT.EQ.KOP(J)) GO TO 20
   10 CONTINUE
      GO TO 30
C
   20 KNOP=J
      NXOP=I
      RETURN
C
   30 CONTINUE
      KNOP=0
      NXOP=0
      RETURN
      END
