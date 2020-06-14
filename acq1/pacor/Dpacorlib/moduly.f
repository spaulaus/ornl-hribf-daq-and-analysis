C$PROG MODULY
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      SUBROUTINE MODULY(KIND,LIST,NUM)
C
      IMPLICIT INTEGER*4 (A-Z)
C
      PARAMETER (TMX=2000)
C
      COMMON/PAC2/ NAMO(4,TMX),KIMO(TMX),CRAT(TMX),
     &               SLOT(TMX),SUBA(TMX),FRED(TMX),FCLR(TMX),
     &               ACLR(TMX),DLAT(TMX),CLAS(TMX),DETN(TMX),
     &     ENTR(TMX),IDNM(TMX),MOTY(2,TMX),USED(TMX),NUMT
C
      INTEGER*4    LIST(20)
C
      CHARACTER*4  USED
C
      SAVE
C
      NUM=0
      DO 100 I=1,NUMT
      IF(KIMO(I).NE.KIND)   GO TO 100
      IF(MOTY(1,I).LE.0)    GO TO 100
      IF(USED(I).EQ.'YES ') GO TO 100
      DO 20 J=1,NUM
      IF(MOTY(1,I).EQ.LIST(J)) GO TO 100
   20 CONTINUE
      NUM=NUM+1
      LIST(NUM)=MOTY(1,I)
  100 CONTINUE
      RETURN
      END
