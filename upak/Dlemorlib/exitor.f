C$PROG EXITOR    - Checks & dismounts all tape units
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/23/2002
C     ******************************************************************
C
      SUBROUTINE EXITOR(N)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*4    N
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
      CALL CKCLI('DMOU')
      CALL CKCLO('DMOU',1)
      CALL CKCLO('DMOU',2)
      CALL CKCLO('DMOU',3)
      STOP
      END
