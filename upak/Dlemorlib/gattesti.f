C$PROG GATTESTI  - Tests a single parameter against a 1-D gate
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      LOGICAL FUNCTION GATTESTI(IG,IX)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/LM30/ GATNAM(20),GATLO(1000),GATHI(1000),GATMX,GATOP,LUGAT
      INTEGER*4    GATNAM,    GATLO,      GATHI,      GATMX,      LUGAT
      CHARACTER*4                                           GATOP
C     ------------------------------------------------------------------
      INTEGER*4    IG,IX
C
      SAVE
C
C     ------------------------------------------------------------------
C
      GATTESTI=.FALSE.
C
      IF(IG.GT.GATMX)     RETURN
C
      IF(IG.LT.1)         RETURN
C
      IF(IX.LT.GATLO(IG)) RETURN
C
      IF(IX.GT.GATHI(IG)) RETURN
C
      GATTESTI=.TRUE.
C
      RETURN
      END
