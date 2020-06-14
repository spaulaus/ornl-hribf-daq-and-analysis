C$PROG GETNUCN   - Gets nucleus name from A & Z
C
C     ******************************************************************
C     BY W.T. Milner at HHIRF - LAST MODIFIED by W.T. Milner 04/25/2002
C     ******************************************************************
C
      SUBROUTINE GETNUCN(A,Z,NUCNAM)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SYMBOL/  BLNK,PNT,GASS,MNS,NUCN(105),ICHR(5)
      INTEGER*4       BLNK,PNT,GASS,MNS,NUCN,     ICHR
C     ------------------------------------------------------------------
      REAL*4          A,Z
C
      INTEGER*4       IA,IZ
C
      CHARACTER*8     NUCNAM
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IA=A+0.5
      IZ=Z+0.5
C
      WRITE(NUCNAM,10)NUCN(IZ),IA
   10 FORMAT(A2,I6)
C
      CALL SQUEZR(NUCNAM,1,8)
C
      RETURN
      END
