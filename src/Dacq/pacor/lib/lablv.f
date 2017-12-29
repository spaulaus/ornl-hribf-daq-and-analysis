
C$PROG LABLV
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 08/24/92
C     ************************************************************
C
      FUNCTION LABLV(LABEL)
C
      INTEGER*4 LABEL,LABL(2)
C
C     ************************************************************
C     RETURNS THE VALUE ASSOCIATED WITH ASCII LABEL CONTAINED IN
C     "LABEL" (!!!! CHECK DIMENSION REQUIREMENTS !!!!)
C     ************************************************************
C
      LABL(1)=LABEL
      LABL(2)='20202020'X
C
      CALL LABLMAN('GET ',LABL,IV,IERR)
C
      LABLV=IV
      RETURN
      END
