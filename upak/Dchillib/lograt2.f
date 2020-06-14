C$PROG LOGRAT2
      FUNCTION LOGRAT2(IA,IB)
C
      CHARACTER*4  KIND
C
      SAVE
C
C     **************************************************************
C     FUNCTION TO CALCULATE LOG-BASE2 OF IA/IB
C     CORRECT ONLY IF RATIO IA/IB OB IB/IA IS INTEGER PWR-OF-2
C     **************************************************************
C
      IF(IA.EQ.IB) GO TO 50
      KIND='POS '
      IRAT=IA/IB
      IF(IB.GT.IA) THEN
                   KIND='NEG '
                   IRAT=IB/IA
                   ENDIF
C
      DO 10 I=1,32
      NT=2**(I-1)
      IF(NT.GE.IRAT) GO TO 20
   10 CONTINUE
      I=32
   20 LTST=I-1
      IF(KIND.EQ.'NEG ') LTST=-LTST
      LOGRAT2=LTST
      RETURN
C
   50 LOGRAT2=0
      RETURN
      END
