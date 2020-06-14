C$PROG VMETFOR   - Formats VMETIM so that it's more readable
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 10/03/2004
C     ******************************************************************
C
      SUBROUTINE VMETFOR
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      COMMON/SC28/ CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM,VMETIMC,CLONOF
      INTEGER*4    CLIDHI,CLIDLO,VMETLO,VMETHI,VMETIM
      CHARACTER*16                                    VMETIMC
      CHARACTER*4                                             CLONOF
C     ------------------------------------------------------------------
C
      CHARACTER*12 TEMPA
C
      CHARACTER*3  TEMPB(4)
C
      EQUIVALENCE  (TEMPB,TEMPA)
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      WRITE(TEMPA,10)VMETIM
   10 FORMAT(I12)
C
      IF(VMETIM.LT.1000)       GO TO 100
      IF(VMETIM.LT.1000000)    GO TO 110
      IF(VMETIM.LT.1000000000) GO TO 120
                               GO TO 130
C
  100 WRITE(VMETIMC,105)TEMPB
  105 FORMAT(4A3)
      RETURN
C
  110 WRITE(VMETIMC,115)TEMPB       
  115 FORMAT(3A3,',',A3)
      RETURN
C
  120 WRITE(VMETIMC,125)TEMPB       
  125 FORMAT(2A3,',',A3,',',A3)
      RETURN
C
  130 WRITE(VMETIMC,135)TEMPB       
  135 FORMAT(A3,',',A3,',',A3,',',A3)
      RETURN
C
      END

C
