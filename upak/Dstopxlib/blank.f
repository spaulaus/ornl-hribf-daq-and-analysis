C$PROG BLANK     - Sets selected portions of array to blank
C
C     ******************************************************************
C     BY T.C. Awes at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE BLANK(ICH,II,IFLG)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SYMBOL/  BLNK,PNT,GASS,MNS,NUCN(105),ICHR(5)
      INTEGER*4       BLNK,PNT,GASS,MNS,NUCN,     ICHR
C     ------------------------------------------------------------------
      INTEGER*4 ICH(80),II,IFLG
C     ------------------------------------------------------------------
C
      IFLG=1
   50 IF(II.GT.60)        GO TO 100
      IF(ICH(II).NE.BLNK) RETURN
      II=II+1
      GO TO 50
  100 IFLG=2
      RETURN
      END
