C$PROG NEWMSK
C
C     ************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 07/01/93
C     ************************************************************
C
      SUBROUTINE NEWMSK(PATNDX,MSK)
C
      INTEGER*4 PATNDX
C
      SAVE
C
      IF(MSK.GE.32768) THEN
                       PATNDX=PATNDX+1
                       MSK=1
                       RETURN
                       ENDIF
C
      MSK=2*MSK
      RETURN
      END
