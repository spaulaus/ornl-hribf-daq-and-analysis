C$PROG QUITTER   - Dismounts all tape units & exits program
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE QUITTER
C
      IMPLICIT NONE
C
      SAVE
C
      CALL CKCLI('DMUL')
      CALL CKCLO('DMUL',1)
      CALL CKCLO('DMUL',2)
      CALL CKCLO('DMUL',3)
      STOP
      END
