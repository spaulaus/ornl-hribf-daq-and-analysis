C$PROG THICK     - Extracts absorber thickness from specifications
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE THICK(ICH,II,IGO,THK)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SYMBOL/  BLNK,PNT,GASS,MNS,NUCN(105),ICHR(5)
      INTEGER*4       BLNK,PNT,GASS,MNS,NUCN,     ICHR
C     ------------------------------------------------------------------
      INTEGER*4       ICH(80),II,IGO
C
      REAL*4          THK
C
      INTEGER*4       JWD(8),KWD(8),IPNT,IJ,I
C
      CHARACTER*32    CKWD
      EQUIVALENCE    (CKWD,KWD)
C
      INTEGER*4      X30,X39
      DATA           X30,X39/'30'X,'39'X/
C
      EXTERNAL       PACK
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IGO=1
      IPNT=0
      DO 10 I=1,8
      JWD(I)=BLNK
   10 CONTINUE
C
      IJ=1
C
      IF(ICH(II).NE.MNS) GO TO 20
C
      JWD(IJ)=ICH(II)
      II=II+1
      IJ=IJ+1
C
   20 IF(ICH(II).NE.PNT) GO TO 30
C
      JWD(IJ)=ICH(II)
      II=II+1
      IJ=IJ+1
      IPNT=1
C
   30 IF(ICH(II).GE.X30.AND.ICH(II).LE.X39) GO TO 40
C
      RETURN
C
   40 JWD(IJ)=ICH(II)
      IJ=IJ+1
      II=II+1
C
      IF(ICH(II).GE.X30.AND.ICH(II).LE.X39) GO TO 40
C
      IF(ICH(II).NE.PNT)                    GO TO 50
C
      IF(IPNT.NE.0) RETURN
C
      IPNT=1
      GO TO 40
C
   50 CALL PACK(JWD,KWD,8)
      CALL SQUEZR(KWD,1,8)
      IGO=2
      READ(CKWD,100)THK
  100 FORMAT(F8.0)
      RETURN
      END
