C$PROG HELPNIT   - Opens lemor.hep
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE HELPNIT(IHEPF)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM11/ LHEP,LMSG,LLOD,LSAV,LSID,LTMP
      INTEGER*4    LHEP,LMSG,LLOD,LSAV,LSID,LTMP
C     ------------------------------------------------------------------
C
      CHARACTER*4  IHEPF
C
      INTEGER*4    NAMHEP(6)
C
      CHARACTER*24 CNAMHEP
C
      EQUIVALENCE (CNAMHEP,NAMHEP)
C
      DATA CNAMHEP/'lemor.hep'/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      LHEP=8
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
