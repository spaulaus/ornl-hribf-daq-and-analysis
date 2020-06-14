C$PROG HELPNIT   - Opens scanor.hep
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 02/03/99
C     ******************************************************************
C
      SUBROUTINE HELPNIT(IHEPF)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC13/ LCON,LCMD,LIN,LBAN,LHEP
      INTEGER*4    LCON,LCMD,LIN,LBAN,LHEP
C     ------------------------------------------------------------------
      INTEGER*4    NAMHEP(6)
C
      CHARACTER*24 CNAMHEP
C
      EQUIVALENCE (CNAMHEP,NAMHEP)
C
      CHARACTER*4  IHEPF
C
      SAVE
C
C     ------------------------------------------------------------------
C
      CNAMHEP='scanor.hep'
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
