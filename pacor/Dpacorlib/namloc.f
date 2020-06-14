C$PROG NAMLOC
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE NAMLOC(NAMLST,NAMTST,NLST,NDX)
C
      INTEGER*4 NAMLST(4,*),NAMTST(4)
C
      SAVE
C
      NDX=0
      DO 20 J=1,NLST
      DO 10 I=1,4
      IF(NAMTST(I).NE.NAMLST(I,J)) GO TO 20
   10 CONTINUE
      NDX=J
      RETURN
   20 CONTINUE
      RETURN
      END
