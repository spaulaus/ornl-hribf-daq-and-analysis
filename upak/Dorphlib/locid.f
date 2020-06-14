C$PROG LOCID     - Searches SPKIO directory for specified ID number
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION LOCID(IDIR,ILO,IHI,IDN)
C
      INTEGER*4 IDIR(*)
C
      SAVE
C
C     ------------------------------------------------------------------
C     FUNCTION TO SEARCH "SPKIO DIRECTORY" FOR ID NUMBERS
C     ------------------------------------------------------------------
C
      IF(IHI.LT.ILO) GO TO 15
      N=2*ILO+1
      DO 10 I=ILO,IHI
      N=N+2
      IF(IDIR(N).EQ.IDN) GO TO 20
   10 CONTINUE
   15 N=0
   20 LOCID=N
      RETURN
      END
