C$PROG KFXTST    - Tests hold-flags for non-linear search operations
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/16/02
C     ******************************************************************
C
      FUNCTION KFXTST(IHOLF,IVAR)
C   
      CHARACTER*4  IVAR
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO DETERMINE IF THE PARAMETER CHARACTERIZED BY
C     IHOLF= 0/1 (0 SAYS FREE / 1 SAYS HOLD)
C     IVAR = "VARIATION CODE"
C     IS TO BE HELD FIXED OR VARIED IN THE NON-LINEAR SEARCH
C     ------------------------------------------------------------------
C   
      IF(IVAR.EQ.'FIX ') GO TO 30
      IF(IVAR.EQ.'UIND'.OR.IVAR.EQ.'ULOC') GO TO 20
      IF(IHOLF.GT.0) GO TO 30
   20 KFXTST=0
      RETURN
   30 KFXTST=1
      RETURN
      END
