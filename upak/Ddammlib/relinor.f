C$PROG RELINOR   - Normalizes relative intensities within fit-region
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/17/02
C     ******************************************************************
C
      SUBROUTINE RELINOR
C
C     ------------------------------------------------------------------
      COMMON/SM28/ RELI(44),NREL,NPKK,KRELF
C     ------------------------------------------------------------------
C
      SAVE
C
C     ------------------------------------------------------------------
C     NORMALIZE RELATIVE INTENSITY WITHIN A "FITTING SECTION" TO
C     GIVE MORE REASONABLE UNCERTAINTIES
C     ------------------------------------------------------------------
C
      RSUM=0.0
      NSUM=0
C
      DO 20 I=1,44
      IF(RELI(I).LE.0.0) GO TO 20
      NSUM=NSUM+1
      RSUM=RSUM+RELI(I)
   20 CONTINUE
      IF(NSUM.LE.0) RETURN
C
      FAC=SQRT(FLOAT(NSUM))/RSUM
C
      DO 30 I=1,44
      RELI(I)=FAC*RELI(I)
   30 CONTINUE
      RETURN
      END
