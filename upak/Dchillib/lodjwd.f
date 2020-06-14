C$PROG LODJWD
      SUBROUTINE LODJWD(IWD,JWD)
C
      INTEGER*4 IWD(20),JWD(32)
C
      INTEGER*4 BLANK
C
      character*4 cblank
      equivalence (cblank,blank)
      DATA      cBLANK/'    '/
C
      SAVE
C
      DO 10 I=1,20
      JWD(I)=IWD(I)
   10 CONTINUE
      JWD(21)=IWD(1)
      DO 20 I=22,32
      JWD(I)=0
   20 CONTINUE
      DO 30 I=26,29
      JWD(I)=BLANK
   30 CONTINUE
      RETURN
      END
