C$PROG HELPNIT   - Opens fitx.hep
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE HELPNIT(IHEPF)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    NAMHEP(6),LHEP
C
      CHARACTER*4  IHEPF
C
      CHARACTER*24 CNAMHEP
C
      EQUIVALENCE (CNAMHEP,NAMHEP)
C     ------------------------------------------------------------------
C
      LHEP=13
C
      CNAMHEP='fitx.hep'
C
      CALL HELPOPEN(LHEP,NAMHEP,IHEPF)
C
      IF(IHEPF.NE.'YES ') RETURN
C
      WRITE(6,30)
      WRITE(6,35)
      WRITE(6,40)
   30 FORMAT(1H ,
     &'Type: h       - for list of HELP code-words & subjects')
   35 FORMAT(1H ,
     &'Type: h all   - for a more detailed help directory')
   40 FORMAT(1H ,
     &'Type: h code  - for command list for associated subject')
C
      RETURN
      END
