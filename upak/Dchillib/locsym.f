C$PROG LOCSYM
      FUNCTION LOCSYM(NAME)
C
      COMMON/CCC/ ISYN(2,100),ISYT(100),ISYD(100),ISYP(100),
     &            ISPF(100),ISYV(16384),NSYM,NSYV
C
      INTEGER*4 NAME(2)
C
      SAVE
C
C     **************************************************************
C     RETURNS LOCATION OF "NAME" IN SYMBOL TABLE "ISYM"
C     RETURNS 0, IF NOT FOUND
C     **************************************************************
C
      DO 10 J=1,NSYM
      IF(NAME(1).EQ.ISYN(1,J).AND.NAME(2).EQ.ISYN(2,J)) GO TO 20
   10 CONTINUE
      LOCSYM=0
      RETURN
C
   20 LOCSYM=J
      RETURN
      END
