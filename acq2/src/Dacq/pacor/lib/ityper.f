C$PROG ITYPER
C
      FUNCTION ITYPER(IBY)
C
      BYTE IBY(8)
C
      BYTE   X20,X30,X39
      DATA   X20,X30,X39/'20'X,'30'X,'39'X/
C
      BYTE   HUC,HLC
      character*1   cHUC,cHLC
      equivalence (cHUC, HUC), (cHLC, HLC)
      DATA   cHUC,cHLC/'H','h'/
C
      SAVE
C
      ILO=0
      IHI=0
      DO 10 I=1,8
      IF(IBY(I).NE.X20) GO TO 20
   10 CONTINUE
      ITYPER=2
      RETURN
C
   20 ILO=I
      DO 30 I=ILO+1,8
      IF(IBY(I).EQ.X20) THEN
                        IHI=I-1
                        GO TO 50
                        ENDIF
   30 CONTINUE
      IHI=8
C
   50 IF(IBY(ILO).GE.X30.AND.IBY(ILO).LE.X39) GO TO 60
      ITYPER=1
      RETURN
C
   60 IF(IBY(IHI).EQ.HUC.OR.IBY(IHI).EQ.HLC) THEN
                                             ITYPER=3
                                             RETURN
                                             ENDIF
C
      ITYPER=2
      RETURN
      END
