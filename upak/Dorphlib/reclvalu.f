C$PROG RECLVALU  - Customizes OPEN's RECL for specific Compiler
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      INTEGER*4 FUNCTION RECLVALU(NBYTES)
C
C     ------------------------------------------------------------------
      IMPLICIT  NONE
C
      INTEGER*4 NBYTES
C
      SAVE
C
C     ------------------------------------------------------------------
C
CX    RECLVALU=NBYTES/4
C
      RECLVALU=NBYTES
C
      RETURN
      END
