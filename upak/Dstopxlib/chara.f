C$PROG CHARA     - Checks character type 
C
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE CHARA(ICH,II,NCHR,IGO)
C
C     ------------------------------------------------------------------
      COMMON/SYMBOL/  BLNK,PNT,GASS,MNS,NUCN(105),ICHR(5)
      INTEGER*4       BLNK,PNT,GASS,MNS,NUCN,     ICHR
C     ------------------------------------------------------------------
      INTEGER*4       ICH(80),II,NCHR,IGO
C
      INTEGER*4       IFLG
C     ------------------------------------------------------------------
C
      IGO=1
      IF(ICH(II).NE.ICHR(NCHR)) RETURN
      II=II+1
      IGO=2
      CALL BLANK(ICH,II,IFLG)
      RETURN
      END
