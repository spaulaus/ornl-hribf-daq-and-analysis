C$PROG GAS       - Looks for GAS specification
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE GAS(ICH,II,IGO,PRS,THK)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SYMBOL/  BLNK,PNT,GASS,MNS,NUCN(105),ICHR(5)
      INTEGER*4       BLNK,PNT,GASS,MNS,NUCN,     ICHR
C     ------------------------------------------------------------------
      INTEGER*4       ICH(80),II,IGO
C
      REAL*4          PRS,THK
C
      INTEGER*4       ITST(4),IGS,IFLG,I
C
      INTEGER*4       BLAN
C
      character*4     cblan
      equivalence     (cblan, blan)
      DATA           cBLAN/'    '/
C
      EXTERNAL        PACK
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IGO=1
C
      DO 10 I=1,3
      ITST(I)=ICH(II+I-1)
   10 CONTINUE
C
      ITST(4)=BLAN
      CALL PACK(ITST,IGS,4)
      IF(IGS.NE.GASS) RETURN
C
      IGO=2
      II=II+3
      CALL BLANK(ICH,II,IFLG)
      CALL THICK(ICH,II,IGO,PRS)
C
      GO TO (30,20) IGO
C
   20 CALL BLANK(ICH,II,IFLG)
      CALL THICK(ICH,II,IGO,THK)
C
      GO TO (30,40) IGO
C
   30 IGO=3
C
   40 RETURN
      END
